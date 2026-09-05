# ADR 0005: Evidence representation

**Status:** Proposed
**Date:** 2026-09-04
**Primitives:** `binder`, `lowering-emit`, `vm-runtime`, `metadata-format` (see
[`primitives.yaml`](../primitives.yaml))
**Roadmap:** [§1.3](../../../ROADMAP.md#13-burn-down-the-diagnostics),
[§2](../../../ROADMAP.md#2-generics)

## Context

[ADR 0004](0004-traits-given-evidence-and-contextual-extensions.md) defines
traits, `given`, context bounds and derivation, and deliberately leaves open what
evidence *is* at runtime. It says only that the elaboration of `T: Eq` into a
parameter is "an implementation detail and is not exposed in normal Panther
source code."

This ADR decides that detail.

Three options were on the table:

1. **Monomorphization** — emit a specialized copy per instantiation, resolve
   constraints at compile time, call directly.
2. **Runtime dispatch on the type tag** — one erased body; at each use, inspect
   the value's `TypeDefToken` and branch to the right instance.
3. **Evidence passing** — resolve the constraint where the type arguments are
   concrete, pass the result in as data, call through it.

Option 2 is rejected outright. `IsInst` can express the branch with no new
opcode, but `checkIsInstance` discriminates the primitives by calling
`metadata.getTypeName` and comparing strings, so every dispatch on an `int` or
`string` key costs a metadata lookup and a string compare — on the innermost loop
of the compiler, since `Dictionary` is a linked list and `_get` is a linear scan.
It is also no more modular than option 1: the generated switch has to know every
type that could flow in, so it needs whole-program knowledge anyway.

Between options 1 and 2 modularity is a wash, which is the argument for
monomorphization: both need whole-program knowledge, so erasure's usual advantage
does not apply. **Option 3 breaks that tie**, because evidence arriving as data
means a constrained generic compiles once and works for a type argument the
compiler has never seen — which is what §3.1's `.pnb` boundary will need.

## Decision

### A. A context bound is a hidden parameter

`K: Eq` elaborates to a parameter carrying the evidence:

```panther
def compute[K: Eq](a: K): bool
```

becomes, conceptually:

```panther
def compute[K](a: K, $ev$K$Eq: Eq[K]): bool
```

This is uniform — methods, functions and constructors all elaborate the same
way. There is no separate mechanism for classes.

Evidence parameters are **appended** after the declared parameters, ordered by
type-parameter declaration order and then by constraint order within a
parameter. Appending rather than prepending keeps every declared parameter at
its existing argument slot, which matters because the VM already reserves slot 0
for the receiver and `getMethodParameterMap` numbers declared parameters from 1
for an instance method ([ADR 0002](0002-binding-this.md), step C). Prepending
would shift every declared parameter and require redoing that map.

The ordering must be deterministic. Stage 3 compares bytecode from two
compilers; evidence parameters emitted in a different order are different
bytecode.

### B. A constrained class stores its evidence in a field

Because the constructor is elaborated like any other method, a constrained class
receives its evidence as a constructor parameter and stores it in a synthesized
field:

```panther
class Dictionary[K: Eq, V](list: List[KeyValue[K, V]])
```

gets a hidden `$ev$K$Eq` parameter on `.ctor` and a hidden field of the same
name. Instance methods reach it through `this`, so `k == key` inside `_get`
lowers to a field read on the receiver followed by a call.

The field falls out of the parameter rule; it is not a second mechanism. The
value is resolved once, at the `new` site, where the type arguments are concrete
— which is the property that makes this cheaper than dispatching per comparison.

Generic code constructing a constrained generic passes its own evidence through.
`Dictionary.contains` already does exactly this in the sources:

```scala
def contains(key: K): bool = {
  list match {
    case List.Cons(KeyValue(k, _), tail) =>
      if (k == key) true
      else Dictionary(tail).contains(key)
  }
}
```

That `Dictionary(tail)` is constructed inside a scope where `K` is abstract, so
the `new` site forwards `this.$ev$K$Eq` rather than resolving anything.

### C. Evidence is a record of method tokens

A `given` compiles to a singleton evidence record whose fields are the method
tokens of its members. One field per constrained parameter regardless of how many
members the trait has.

A call through evidence is `Ldfld` (the evidence), `Ldfld` (the member's token),
`Calli`. For a trait with one or two members, flattening to one field per member
and skipping the record is a valid emitter choice; it does not change the
semantics.

Evidence singletons are static fields initialised in `$runtimeInit`, which
already exists and already runs the static-constructor list the binder collects
in `staticCtors`.

Recursive evidence works because the records are values: `Eq[List[Symbol]]` holds
a reference to `Eq[Symbol]`. Resolution builds a graph, and a goal already in
progress resolves to the record being built — the same in-progress rule ADR 0004
requires for derivation.

### D. `Calli` is the one new opcode

```scala
case Opcode.Calli =>
  val token = MethodToken(popInt())
  methodCall(token, ip)
```

`Call` already reads its token from the instruction stream and hands it to
`methodCall`; `Calli` pops it from the stack and calls the same function. No
new call machinery, no vtable, no `Callvirt`.

This is the entire VM cost of the design. Evidence passing is easy to rule out
on the grounds that it needs function values and an indirect call; neither
objection holds. Evidence is compiler-internal and never becomes a user-visible
closure, and the indirect call is these two lines.

#### Why an indirect call is unavoidable here

Inside `Dictionary._get`, `k == key` has to become a call to *some* `equals`.
Which one depends on `K`, and under A the body is compiled once for every `K`, so
the token lives in the evidence record that arrived as a parameter — on the
stack, not in the instruction stream. `Call` can only read its operand from the
stream. Given evidence passing, the target is a value, and calling a value needs
an instruction that takes one.

The two ways to avoid `Calli` are both worse:

- **`Callvirt` and vtables.** Real machinery, and the object model has no vtables
  to dispatch through.
- **A static dispatcher per trait member**, taking the evidence and branching on
  an id to pick the implementation. It needs no new opcode, but the VM has no
  jump table, so the branch is built from `Ceq`/`Brtrue` and costs O(N) per call
  — and the dispatcher is a whole-program switch that regenerates whenever a type
  is added. That is the runtime-dispatch option this ADR already rejected,
  wearing a different hat.

#### `Calli` goes cold under full specialization

Worth stating plainly: **if every generic is monomorphized, `Calli` is never
reached.** Panther is whole-program today — no `.pnb` reader, no separate
compilation — so every instantiation is statically known and the emitter could
specialize all of them. The opcode becomes load-bearing at the same moment §3.1
does, when a type argument arrives that the compiler has not seen.

That is an argument about sequencing, not about the design. Evidence passing is
kept as the mechanism because it is the *smaller* change to the compiler:

| | Evidence passing | Always monomorphize |
| --- | --- | --- |
| Emitter token model | unchanged | rekey `methodTokens`, `fieldTokens`, `typeTokens` on (Symbol, typeArgs) |
| New pass | none | reachability walk over instantiations |
| Stage 3 | unaffected | walk order must be deterministic or the bytecode differs |
| Divergence | none | polymorphic recursion needs a depth limit and a diagnostic |
| VM | `Calli`, two lines | none |

`Calli` is what buys the four rows above it. It is also why the implementation
order should be evidence passing first and specialization second: with no
specialization, `Calli` runs on every constrained call and is exercised from the
first test, rather than being added as an opcode nothing reaches.

### E. Constraints live in `Type.GenericFunction.traits`

The type model already has the slot.
`Type.GenericFunction(location, generics, traits, parameters, returnType)`
carries `traits: List[Type]`, and every construction site in `Binder` passes
`List.Nil`. Nothing reads it; `Types.substitute` substitutes through it and
`AstPrinter` prints it.

Constraints go there. This keeps the *declared* parameter list clean, which
matters for the hazard in the next section.

### F. Elaboration happens after binding

The binder records constraints on the type and resolves them at each use site.
Rewriting the parameter list into its elaborated form happens in lowering, so
that everything type-directed sees the declared signature.

### G. Monomorphization becomes an optimization, not the semantics

Specializing a generic for a known instantiation stays available and is now a
pure emitter concern: it replaces a `Ldfld`/`Calli` with a direct `Call`, never
changing what the program means. Decoupling them this way means the language is defined without a
specialization pass, so stage 3's determinism does not depend on that pass's
traversal order.

## Hazards

**Arity diagnostics must use the declared arity.** There are six
`reportArgumentCountMismatch` call sites in `ExprBinder`. If evidence parameters
were visible to them, every call to a constrained generic would report a
mismatch. Keeping constraints in `traits` until lowering (E and F) is what
prevents this.

**`getMethodParameterMap` filters on `SymbolKind.Parameter`.** Evidence symbols
need argument slots, so they must either carry that kind or be handled explicitly
there. This is the mirror image of the `SymbolKind.This` problem ADR 0002 hit:
`This` needed to be *skipped* in metadata emission so it would not add a spurious
parameter row. Evidence needs the opposite — a slot, but still distinguishable
from a declared parameter for printers and tooling. A `SymbolKind.Evidence` that
`getMethodParameterMap` treats as a parameter is the smaller change.

**Object layout changes.** A constrained class gains a field and a constructor
parameter, so `FieldTable` and `ParamTable` grow rows and every `new` site emits
an extra argument.

**Constraints are viral.** Adding `[K: Eq]` to `Dictionary` propagates to
everything generic that constructs one. `DictionaryModule.empty[K, V]()` is
`new Dictionary[K, V](List.Nil)` today and would need `[K: Eq, V]`. This is a
real cost of explicit bounds and it is worth measuring on the sources before
committing.

**An indirect call cannot be inlined.** Evidence passing has a performance
ceiling that monomorphization does not. G is what keeps the ceiling liftable.

## Alternatives considered

**Monomorphization as the semantics.** Rejected as the
*definition* while kept as an optimization. Deciding it here would tie the
language to a whole-program pass, forbid a type argument the compiler has not
seen, and put stage-3 bytecode identity at the mercy of the pass's instantiation
order.

**Runtime dispatch on the type tag.** Rejected — see Context. The decisive
detail is that `checkIsInstance` compares type *names* as strings for the
primitives, so the cheap-looking option is expensive exactly where the compiler
is hottest.

**A singleton object per given, called virtually.** The natural object-oriented
shape, and it needs a vtable and `Callvirt`. Rejected: `Calli` over a token
record gets the same dispatch for two lines of VM and no change to the object
model.

**Evidence as a field only, not a parameter.** Would cover constrained classes
and nothing else — `def contains[T: Eq](…)` has no object to hang a field on.
The parameter rule subsumes it.
