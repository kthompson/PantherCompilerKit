# ADR 0001: Generic type inference

**Status:** Proposed
**Date:** 2026-09-03
**Primitives:** `binder` (see [`primitives.yaml`](../primitives.yaml))
**Roadmap:** [§2.1](../../../ROADMAP.md#21-type-argument-inference-through-call-chains)

## Context

Generics are the largest single source of self-hosting diagnostics. Of the
982 diagnostics `pncs` reports over `pnc/src` when run with no diagnostics
limit, 581 mention an unsolved type variable (`$0`, `$1`, …). Grouped by
shape:

| Count | Shape                                                   | Example                                                         |
| ----: | ------------------------------------------------------- | --------------------------------------------------------------- |
|   165 | `Cannot convert from $N to C`                           | `Cannot convert from $0 to MemberSyntax`                        |
|   148 | `Symbol X not found for type $N`                        | `Symbol closeBrace not found for type $0`                       |
|   113 | `Cannot convert from C<$N> to C<C>`                     | `List<$0>` to `List<MemberSyntax>`                              |
|    56 | `Cannot convert from C<C> to C<$N>`                     | `List<string>` to `List<$0>`                                    |
|    28 | `Cannot convert from C<any, any> to C<C, C>`            | `Dictionary<any, any>` to `Dictionary<Symbol, BoundExpression>` |
|    22 | `Cannot convert from C<any> to C<C>`, `C<any> to C<$N>` | `List.Cons<any>` to `List<string>`                              |

(`sbt pnc/compile` reports 996 for the same sources; the breakdown here is
from the direct run.)

### How generics are represented

- A type parameter is `Type.Variable(location, id)` where `id` is the
  parameter's **position** in the declaring class's or function's parameter
  list ([`Binder.scala`](../../../pncs/src/main/scala/Binder.scala),
  `genericTypeParamAsType`). `T` in `class Foo[T]` and `T` in
  `def id[T](x: T)` are both `$0`.
- A generic class is `Type.GenericClass(…, args: List[GenericTypeParameter], symbol)`
  until it is applied; `Foo[int]` binds to `Type.Class(…, args = [int], symbol)`.
- An enum is `Type.Alias(…, args, value = Type.Union(cases), symbol)`. Each
  case with parameters is a `Type.GenericClass` that **shares the enum's
  parameter list**, so `List.Cons` is `GenericClass(Cons, [T])` and its
  constructor is `GenericFunction([T], (head: $0, tail: List<$0>))`. A case
  with no parameters is a plain `Type.Class` with no arguments.
- Every enum in the stdlib and the compiler is an alias: `List[+T]`,
  `Option[+T]`, `Result[+A, +B]`, `Either[+A, +B]`, `Chain[T]`. Variance is
  recorded on `GenericTypeParameter.variance` and not used.
- Inference lives in
  [`TypeInference.scala`](../../../pncs/src/main/scala/TypeInference.scala).
  [`Inference.scala`](../../../pncs/src/main/scala/Inference.scala) is
  commented out in its entirety; it is a unification design that panics on
  mismatch and whose `instantiate` is `???`.

### The gaps

Each of the shapes above traces to one of five causes.

**1. Pattern variables never receive the scrutinee's type arguments.**
`inferMatchCase` and `checkMatchCase` bind each case with `bindPattern`,
which takes no expected type. `bindPatternWithType` exists but its
`Extract` branch delegates straight back to `bindPattern`. The extract path
reads the constructor's parameter types raw through
`getFunctionParameterTypes`, so in

```
list match {
  case List.Cons(head, tail) => f(head)
}
```

`head` is `$0` and `tail` is `List<$0>` whatever the type of `list`. This is
the 165 + 148 rows, and most of the 113: the variable's type flows into a
call or a member access and fails there.

**2. Substitution is partial, and there are three of them.**

| Function                                     | Handles                             |
| -------------------------------------------- | ----------------------------------- |
| `TypeInference.substituteTypeVariable`       | `Variable`, `Function`              |
| `TypeInference.substituteGenericTypesInType` | `Variable`, `Class`                 |
| `ConversionClassifier.substituteTypeArgs`    | `Variable`, `Class`, `GenericClass` |

None handles `Type.Alias`, which is every enum. Calling
`List.Cons(arg, sourceFiles)` infers `$0 = string` from `head`, then
substitutes into `tail: List<$0>` — an alias — and leaves it untouched. That
is the 56 `C<C> to C<$N>` rows. None handles `Union` either, and the two
in `TypeInference` each miss what the other covers.

**3. Inference is partial.** `inferTypeFromPair` learns from a bare
`Variable` and from `Class` against `Class`. It learns nothing from an
alias against an alias (`List<$0>` against `List<string>`), from a case
against its enum (`List.Cons<int>` against `List<$0>`), or from function
types.

**4. Constructor calls give up on more than one parameter.** The
`GenericClass` branch of `inferCall` uses `inferTypeArgumentsFromConstructor`,
which handles exactly one type parameter and otherwise fills every slot with
`any` — the 28 `Dictionary<any, any>` and `Result.Error<any, any>` rows. In
`inferNew`, explicit type arguments are bound and then overwritten:
`new Dictionary[K, V](List.Nil)` infers from `List.Nil`, learns nothing, and
produces `Dictionary<any, any>`.

**5. Conversions ignore variance.** `semanticTypeEquals` requires type
arguments to match exactly, so `Result.Error<E, never>` is not convertible
to `Result<E, B>` even though `Result` is declared `[+A, +B]`. An unsolved
parameter defaults to `any` regardless of its variance, which cannot be
assigned to anything narrower.

### A constraint to keep in view

Because ids are positional, a generic method inside a generic class would
collide: in `class Foo[T] { def bar[U](x: U): T }` both `T` and `U` are `$0`.
No such method exists today — every generic method in `pncs/`, `runtime/`,
`metadata/` and `text/` is on an `object` — so the positional scheme is
sufficient for self-hosting. It is a limit of the language as implemented,
not of the plan below.

## Decision

Fix the five causes in the order that lets each step be measured on its own
with `sbt pnc/compile`. Each step is a commit; the count must not rise after
any of them. Each step gets a `TypeTests` case that fails before it and
passes after.

**A. One substitution.** Add `TypeInference.substitute(typ, typeArgs)` that
maps `Variable(id)` to `typeArgs(id)` (or itself when out of range) and
recurses through `Class` and `Alias` arguments, `Union` cases, `Function`
parameters and return type, and `GenericFunction` parameters and return type
(its own generic list is left alone). Replace the bodies of the three partial
functions with calls to it; `ConversionClassifier.substituteTypeArgs` keeps
only its `GenericClass → Class` mapping and delegates the rest. Then delete
the two `TypeInference` partials.

**B. Inference covers every type constructor.** `inferTypeFromPair` learns
from:

- `Alias` against `Alias` — argument lists pairwise;
- a case `Class` against its enum's `Alias`, and the reverse — argument lists
  pairwise, which is correct because a case's parameter list _is_ the enum's;
- `Function` against `Function` — parameters pairwise, then return type;
- `Union` — skipped, explicitly, with a comment. A union scrutinee carries no
  single argument list to learn from.

**C. Patterns carry the scrutinee type.** `checkMatch` and
`inferMatchExpression` pass `getType(matchedExpr)` into the case binders,
which pass it to a single `bindPattern(pattern, scope, expectedType)`; the
no-type overload goes away. The `Extract` branch resolves the constructor,
takes its parameter types, and substitutes `typeArgsFor(expectedType, caseSymbol)`:

| `expectedType`            | Type arguments                                              |
| ------------------------- | ----------------------------------------------------------- |
| `Alias(_, args, _, _)`    | `args`                                                      |
| `Class(_, _, _, args, _)` | `args`                                                      |
| `Union(cases)`            | the `args` of the case whose symbol is `caseSymbol`, if any |
| anything else             | none — parameters bind as declared                          |

Sub-patterns recurse with their substituted parameter type. `Identifier`
binds to `expectedType`, `TypeAssertion` binds its inner pattern to the
annotated type, `Type` and `Discard` bind nothing — all as now.

**D. Constructors infer like calls.** Every constructor path — `inferNew`'s
`Class` and `GenericClass` branches, `inferCall`'s `GenericClass` branch —
uses `inferTypeArgumentsFromCall`. `inferTypeArgumentsFromConstructor` and
its helpers are deleted. Type arguments written at the call site win: when
`new X[A, B](…)` binds to a `Class` with arguments, those are the arguments,
and inference runs only for `new X(…)`. `checkNew`, and a new `GenericClass`
branch in `checkCallLHS`, pass the expected type through
`checkTypeArgumentsFromCall` so `val d: Dictionary[K, V] = new Dictionary(List.Nil)`
solves from the left-hand side, as `checkGenericFunctionCall` already does
for functions.

**E. Variance is used, not just recorded.** An unsolved parameter defaults
by its declared variance: covariant → `never`, contravariant → `any`,
invariant → `any` (unchanged). `ConversionClassifier.classify` gains a case
for two `Type.Class` values with the same symbol: look up the symbol's
`GenericClass` parameter list and compare arguments pairwise — covariant
argument must convert forwards, contravariant backwards, invariant must be
semantically equal. A successful comparison is `Conversion.Identity`,
matching the existing union-to-alias rule, so no `Cast` is inserted and the
lowerer and emitter are untouched. `ExprBinder.isSubtype` gets the same rule
so `subsume` and `bindConversion` agree.

Throughout: no new exceptions — every failure is a diagnostic
(`diagnostics-not-exceptions` in `primitives.yaml`); no `==` on class types
and no re-wrapping of `Either`/`Option` values in the new code, because the
self-hosted binder cannot express them yet; transpile after every step.

## Consequences

- A, B and C together address the 581 diagnostics that mention a type
  variable; D addresses the ~50 `<any, …>` rows; E is what lets D's `never`
  default assign, and it also makes `val xs: List[int] = List.Cons(1, List.Nil)`
  check in inference position rather than only against an annotation.
- Variance is _used_ by conversions but still not _enforced_ on
  declarations — a covariant parameter in a parameter position is not yet a
  diagnostic. That stays ROADMAP §2.4. Upper bounds (§2.2) are untouched.
- Positional ids stay. Generic methods on generic classes remain unsupported
  and must be caught: when the binder sees one it reports a diagnostic
  rather than silently aliasing the ids. Scoping ids properly is a
  follow-up ADR.
- `Inference.scala` stays commented out and is not the basis for this work;
  see the alternatives below. Removing the file is a separate, mechanical
  change.
- Diagnostics outside this ADR's scope, in the same run: `==` and `!=`
  between an enum type and one of its cases (51, e.g.
  `Option<string> == Option.None`), `+` with a non-string operand (34),
  `Type X not defined` (38), `Symbol ??? not found` (24), `Invalid namespace`
  (18). None involve type variables.
- ROADMAP §2 states that `Inference.scala` is a real implementation. It is
  not; §2 should point here instead, and its diagnostic counts move as each
  step lands.

## Alternatives considered

**Revive `Inference.scala`.** It is a proper unifier with an occurs check,
and the shape of the problem — flowing types through nested constructors —
is what unification is for. It is rejected for now because it panics on every
mismatch, which violates `diagnostics-not-exceptions`; its `instantiate` is
unwritten; and it keeps mutable state in a `Dictionary` the self-hosted
binder would have to support. The positional scheme handles every generic
in the current sources, so the smaller change is the right first move. If
scoping ids for nested generic methods becomes necessary, that is the point
to revisit unification.

**Name-keyed type parameters.** `Type.scala` carries a commented-out
`Type.Generic(location, name, variance, upperBound)`. Keying by name fixes
the positional collision but shifts the problem to shadowing and to every
substitution site knowing which declaration a name belongs to. Deferred with
the follow-up ADR on scoped ids.

**Default every unsolved parameter to `any`.** Simplest, and what the
constructor path does today. Rejected: an `any` argument cannot be assigned
to a narrower one, so `Result.Error(e)` can never satisfy a declared
`Result[E, B]`. With the covariance the stdlib already declares, `never` is
the type that assigns everywhere it should.
