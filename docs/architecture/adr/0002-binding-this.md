# ADR 0002: Binding `this`

**Status:** Accepted — implemented 2026-09-03, see [Outcome](#outcome)
**Date:** 2026-09-03
**Primitives:** `binder`, `lowering-emit` (see [`primitives.yaml`](../primitives.yaml))
**Roadmap:** [§1.3](../../../ROADMAP.md#13-burn-down-the-diagnostics)

## Context

`this` is the largest single item in the self-hosting diagnostics: 35 of the
298 `pncs` reports over `pnc/src` are `Symbol this not found`. Every one is
the same cause. The binder never defines a symbol for `this`, so the
identifier resolves to nothing.

### `this` is an ordinary identifier today

There is no `ThisKeyword` in
[`SyntaxKind`](../../../pncs/src/main/scala/SyntaxKind.scala), so the lexer
scans `this` as an `IdentifierToken` and the parser produces an
`Expression.IdentifierName` like any other name. Binding reaches
`ExprBinder.bindIdentifier`, which calls `scope.lookup("this")`, finds
nothing, and reports. Nothing upstream of the binder needs to change.

### The back end is already built for it

Everything downstream of binding already assumes a receiver exists:

- `LoweredExpression.This` is a case in
  [`Lowered.scala`](../../../pncs/src/main/scala/Lowered.scala), and the
  lowerer already constructs one when it rewrites an implicit-receiver field
  assignment.
- `Emitter.emitThisExpression` emits `Ldarg0`.
- `MethodMetadata.hasThis` is set to `!isStatic` for every method
  (`emitMethodMetadata`).
- The VM reserves the slot: `argsp = sp - numArgs - hasThis - 3`, and
  `emitCallExpression` pushes the receiver before the arguments, so the
  receiver is argument slot 0.

So the missing piece is a symbol, a type for it, and one dispatch case in the
emitter.

### The shapes that have to work

The 35 sites are not all method bodies. Grouped by context:

| Context               | Example                                           |
| --------------------- | ------------------------------------------------- |
| Method body           | `def get(): T = this match { … }` (`Option`)       |
| Field initialiser     | `val length: int = this match { … }` (`List`)      |
| Class-body statement  | `new ConversionClassifier(this)` (`Binder`)        |
| Member access         | `Math.max(this.end, span.end)` (`TextSpan`)        |
| Argument position     | `Option.Some(this)` (`Symbol`)                     |

A field initialiser and a class-body statement are bound in the class's own
scope, not a method scope, so a design that attaches `this` to methods alone
would miss nine of the 35.

### The type of `this`

Three cases, from what the binder already stores for the enclosing symbol:

| Declaration      | `getSymbolType(symbol)`         | Self type wanted           |
| ---------------- | ------------------------------- | -------------------------- |
| `class Foo`      | `Type.Class(…, Nil, sym)`       | the same                   |
| `class Foo[T]`   | `Type.GenericClass(…, args)`    | `Type.Class(…, [$0], sym)` |
| `enum Option[T]` | `Type.Alias(…, Nil, union)`     | the same                   |

Only the generic-class case needs work: an uninstantiated `GenericClass` has
to be applied to its own parameters as variables, exactly as
`ExprBinder.genericsAsVariables` already does for constructors. A generic
enum's alias carries `List.Nil` for its arguments, and its cases share the
enum's parameter list, so `this match { case Option.Some(value) => value }`
already substitutes to `$0` and agrees with the declared return type `T`.

### Parameter indices are already wrong

Binding `this` runs straight into a latent bug. The emitter numbers a
method's declared parameters from 0 (`getMethodParameterMap`), but the VM
gives slot 0 to the receiver whenever `hasThis`, so declared parameters
actually begin at 1. `Ldarg0` inside an instance method loads the receiver,
not the first parameter.

This is observable today, without any `this` in the source:

```panther
class Foo() {
  def bar(x: int): int = x + 1
}
new Foo().bar(41)
```

evaluates to `1`, not `42`: `x` loads the receiver, which reads as `0`.

No enabled test covers it. Every multi-parameter method in `VmTests` is
declared on an `object`, which is static and has no receiver, and the two
tests that would have caught it — "execute classes with args" and "access
class fields via constructor" — are commented out.

Binding `this` does not cause this bug, but it makes it reachable from
ordinary code, and any test of `this` alongside a parameter would fail for
this reason rather than its own. It is in scope here.

## Decision

Four steps, each measured with `sbt pnc/compile` and committed separately.

### A. Define a `this` symbol on each class and enum

Add `SymbolKind.This`. When the binder binds a class or enum declaration,
define one child symbol named `this` on that class's symbol, kind `This`,
typed as the self type from the table above.

Attaching it to the class rather than to each method is what makes the field
initialiser and class-body cases work: `Symbol.lookup` already walks up the
parent chain, so a method scope, a block scope, and the class scope all reach
the same symbol. It also means `bindIdentifier` needs no special case —
`this` resolves through the ordinary path and produces a
`BoundExpression.Variable` like any other name.

Objects get no such symbol. An object is static, its methods have no
receiver, and `this` inside one stays a diagnostic.

### B. Emit it

`Emitter.emitVariable` dispatches on symbol kind and panics on anything
unexpected; add `SymbolKind.This` there, emitting `Ldarg0`. Because
`emitLHS` routes its `Variable` case through `emitVariable`, this one case
covers both expression position (`Option.Some(this)`) and receiver position
(`this.end`).

Nothing needs to change in the lowerer: the `this` symbol travels as an
ordinary `LoweredExpression.Variable` and is recognised by kind at the end.
`LoweredExpression.This` keeps its existing role in the implicit-receiver
path.

Metadata emission must skip `SymbolKind.This`, or it would add a spurious
parameter row to the class and the VM would double-count the receiver.

### C. Start declared parameters at 1 when the method has a receiver

`getMethodParameterMap` takes a starting index; pass 1 for a method with
`hasThis` and 0 for a static one. The receiver then occupies slot 0 for both
the emitter and the VM.

### D. Re-enable the two commented-out `VmTests`

"execute classes with args" and "access class fields via constructor" are
the regression tests for step C. Add tests for `this` itself: an instance
method returning `this`, a method reading a field through `this`, and a
method that uses `this` alongside a declared parameter.

## Consequences

`this` becomes an ordinary symbol that user code could shadow with a local
named `this`. The parser allows it because `this` is not a keyword. This is
accepted for now; making `this` a keyword is a lexer and parser change, and
the diagnostics do not currently justify it.

A method on an object nested inside a class will find the outer class's
`this` through the parent walk, which is wrong. No such nesting exists in the
sources. Stopping the walk at an object boundary is a follow-up if one
appears.

Step C changes generated bytecode for every instance method with parameters.
Since that bytecode is currently wrong, and the transpiled twin is compared
by source rather than by bytecode, nothing else has to move with it.

## Alternatives considered

**Make `this` a keyword with its own expression node.** A `ThisKeyword`, an
`Expression.This`, a `BoundExpression.This`, and cases in every printer and
walker. It is the shape a mature compiler wants, and it removes the shadowing
hole. Rejected for now because it touches the lexer, parser, AST, printers and
transpiler to fix diagnostics that a symbol fixes on its own. The symbol can
be replaced by a node later without changing what binds.

**Define `this` as a `Parameter` symbol on each method.** It would fall into
the existing parameter-index machinery and get slot 0 for free. Rejected: it
misses field initialisers and class-body statements, it duplicates a symbol
per method, and it would emit parameter metadata that the VM already accounts
for separately through `hasThis`.

**Resolve `this` specially in `bindIdentifier`** by walking the scope chain to
the nearest class at each use. Equivalent in effect, but it puts the rule in
the expression binder instead of the symbol table, and every consumer
downstream still needs a symbol to refer to.

## Outcome

Landed as one commit.

| Step                                         | Diagnostics |
| -------------------------------------------- | ----------: |
| baseline                                      |         298 |
| A + B + C — bind `this`, emit it, fix slots   |         271 |
| on-demand member typing (see below)           |         282 |

All 35 `Symbol this not found` are gone. 278 tests pass, up from 274.

**The number went up at the end, and that is the right outcome.** Making
`this` usable meant fixing `bindMemberForSymbolAndType`, which read a
member's type with `tryGetSymbolType` and produced an untyped error
expression when the member had not been typed yet. A member referenced from
inside its own type — `this.x` in a method of the class that declares `x` —
hits that case, and the lowerer panicked on the error expression rather than
reporting anything. It now types the member on demand through
`getSymbolType`.

The 11 new diagnostics are all consequences of members that now resolve.
`binder.anyType` has no type annotation, so every `toType == binder.anyType`
in the classifier previously compared against an unknown and reported
nothing; it now types as `Type.Any` and reports the `==`-between-enum-and-case
gap that is already item 2 on the burn-down list. Fewer silent unknowns, more
precise diagnostics against a gap that was already counted elsewhere.

### What this uncovered but did not fix

The two commented-out `VmTests` stayed commented. Both fail for defects that
predate `this` and are independent of it, and the comment now names them:

- **An implicit field read pushes no receiver.** `emitField` emits `Ldfld`
  with nothing on the stack, so `x + y` inside a method fails at runtime
  where `this.x + this.y` succeeds. The natural fix, now that `this` binds,
  is for the binder to resolve a bare identifier that lands on a non-static
  field into a member access on `this`.
- **A class with no template gets no constructor body.** Its method address
  stays -1 and the VM reads past the end of the chunk.

Also newly reachable, because enum methods now bind where they previously
failed on `this`: enum method emission does not work. `emitMemberAccess`
panics on a receiver of kind `Class` (a case), and member lookup on a case
type does not consult its enum. Both were unreachable before.

### Deviations

Every step landed as one commit rather than four. They are interlocked:
binding `this` without the emitter case panics, the parameter-slot fix has no
test that passes without a receiver to test it against, and `this.x` panics
the lowerer without the member-typing fix.

Step D is partial. The two commented-out `VmTests` stayed commented, for the
reasons above. In their place are four tests that do pass: two for the
parameter slots, and two for `this` reading a field, one of them alongside a
declared parameter.

Step C reads `MethodMetadata.hasThis` rather than recomputing staticness.
`Symbol.isStatic()` and the emitter's `parentStatic` disagree for a method
declared directly in a namespace — the first says instance, the second
static — and only `hasThis` is what the VM actually uses.

The self type applies the type's own parameters for a generic enum as well
as a generic class. Without it, `Chain[T].concat` returning `this` reported
`Cannot convert from Chain to Chain<$0>`, because an uninstantiated enum
alias carries `List.Nil` for its arguments.

Typing a member on demand can recurse if two members refer to each other.
The old code could not, because it gave up instead. Nothing in the sources
or the tests triggers it, and the same exposure already exists wherever
`getSymbolType` is called during binding.
