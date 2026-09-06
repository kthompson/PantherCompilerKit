# ADR 0006: Conditional givens

**Status:** Accepted — implemented 2026-09-05, see [Outcome](#outcome)
**Date:** 2026-09-05
**Primitives:** `binder`, `lowering-emit` (see [`primitives.yaml`](../primitives.yaml))
**Roadmap:** [§1.3a](../../../ROADMAP.md#13a-burn-down-the-derivation-reports)

## Context

A conditional given is one whose head holds only because a premise does:
`Eq[List[T]]` given `Eq[T]`. It is the single largest item on the board — 62 of
the 105 unprovable derivation parameters are a generic container
([ROADMAP §1.3a](../../../ROADMAP.md#13a-burn-down-the-derivation-reports)),
and it is what
[ADR 0004](0004-traits-given-evidence-and-contextual-extensions.md) and
[ADR 0005](0005-evidence-representation.md) both record as still not built.

### What already works

Most of the front half is done. `given [T: Ord] => Ord[Box[T]] { … }` parses,
binds, and registers with its head applied to its own type variable —
`Ord<Box<$0>>` — with the context bound kept as a constraint on `BoundGiven`.
It takes part in the coherence check by unification, so `Ord[Box[int]]` and
`Ord[Box[T]]` are correctly rejected as overlapping. And `resolveEvidence`
already discharges premises recursively: a call needing `Ord[Box[int]]` matches
the conditional given, substitutes `T := int`, and goes looking for `Ord[int]`
behind it, reporting the *inner* goal when that is what is missing.

`Evidence` is already a tree — `symbol`, `head`, `typeArguments`, and
`dependencies` — so the proof that comes back from resolution is the whole
derivation, not just the top given.

### What is missing

The premise never reaches the given's own body. A body that writes
`a.value.compare(b.value)` reports `compare` as not found on `T`, because two
separate things are absent and neither is useful without the other:

1. **The record cannot hold a reference to another record.**
   [ADR 0005](0005-evidence-representation.md) chose `Array[int]` for the
   evidence record — one slot per trait member, holding that member's method
   token — precisely because `Newobj` runs a constructor and a named record
   would need a synthesized type *and* a synthesized constructor per given. The
   outcome section already names what that gave up: "the dependency slot".

2. **A given's members have no evidence parameter.**
   `defineEvidenceParameters` runs for methods and constructors, never for a
   given. Turning it on alone would not help, because there is nowhere to pass
   the argument from: at the point of call the only thing the caller holds is
   the record.

There is also a third thing, not recorded anywhere and only visible once the
first two are fixed: **records are laid out one per given**. `evidenceFields` is
keyed by the given's symbol, and `$runtimeInit` stores one record into one
static field for each. A conditional given has no single record — `Eq[List[int]]`
and `Eq[List[Symbol]]` share their members' tokens but differ in what they point
at underneath.

## Decision

### A. The record becomes `Array[any]`, tokens then dependencies

Layout, for a given whose trait has `m` members and whose premise list has `k`
constraints:

| slot | holds |
| --- | --- |
| `0 … m-1` | the method token of the given's implementation of trait member *i* |
| `m … m+k-1` | the evidence record proving constraint *j* |

The token half is unchanged, so every existing reader keeps working: a `Calli`
site indexes by the member's position in the trait and never looks past `m`.

`Array[any]` rather than `Array[int]` costs nothing at runtime. The VM's array
is untyped — `Newarr` reads an element type token only to pick a default value,
and `Stelem`/`Ldelem` write and read a `Value` at `heap(addr + index)` without
consulting it. The change is a type token in the emitter and a `Type` in the
binder.

### B. One record per ground goal, not per given

`evidenceFields` is rekeyed from the given's symbol to the ground goal it
proves. A given with no premise still yields exactly one record, so nothing
about the existing shape changes; a conditional given yields one per
instantiation reached.

Instantiations are interned during binding, as resolution discovers them. Every
node of the `Evidence` tree `resolveEvidence` returns is a ground goal, and each
becomes a record. Nothing is emitted for a goal no code asks for.

### C. Every given member takes the record as a trailing evidence parameter

One extra parameter, `$ev$self`, of the record type, on every method of every
given — conditional or not. A member that has no premise ignores it.

Uniformity is the whole point, and it is forced by `Calli`. The VM computes a
call frame's base as `sp - numArgs - hasThis - 3`, reading `numArgs` from the
*callee's* metadata, so a caller that pushes a different number of arguments
than the callee declares misaligns the frame. A `Calli` inside
`def f[T: Eq](a: T, b: T) = a == b` dispatches on a token read out of whatever
record was passed in — which may have come from any given at all. The caller
cannot know whether that given has premises, so the arity cannot depend on it.

Passing the record rather than the premises themselves is what makes one
parameter enough for any number of premises.

### D. A premise resolves to a dependency slot of `$ev$self`

Inside a given's body, `Ord[T]` is discharged by reading slot `m + j` of
`$ev$self`. This is the same contextual-extension rule ADR 0004 already
describes for `def same[T: Eq]` — `a.compare(b)` resolves `compare` through the
applicable evidence — with a different source for the evidence value.

Concretely, `scanEvidenceMembers` already finds evidence by walking a symbol's
members and testing the *type*, not the kind: "what makes a symbol evidence is
that it holds a trait applied to a type, not which slot it arrived in". A
premise is not a symbol of its own, so it needs a bound node that says *slot j
of this record* rather than *this symbol*.

### E. Records are allocated first and filled second

`$runtimeInit` builds every record in two passes: `Newarr` and `Stsfld` for all
of them, then the slot stores.

This is not an optimisation, it is what makes cycles terminate. `Eq[Symbol]`
holds a `List[Symbol]` of children, so it depends on `Eq[List[Symbol]]`, which
depends back on `Eq[Symbol]`. ADR 0005 says "a goal already in progress resolves
to the record being built", and with arrays that falls out for free: the
reference stored in a dependency slot is to an array that already exists and is
not yet populated. No ordering pass, no topological sort, no cycle detection.

## Alternatives considered

| alternative | why not |
| --- | --- |
| **Specialize the given per instantiation.** Clone `Ord[Box[T]]`'s members with `T := int` and let the premise become an ordinary direct call. No record change, no extra parameter | needs body cloning with type substitution, which the compiler has nowhere else and would have to grow for this alone. ADR 0005 decision G keeps monomorphization available as an *optimization*; making it the semantics is the thing that decision declines |
| **A global record table; a dependency slot holds an `int` index into it.** Keeps `Array[int]` | the member still has to receive its own record to read the index out of, so decision C is unchanged and a table is added on top. Strictly more machinery |
| **One evidence parameter per premise, rather than the record.** `compare(a, b, $ev$T$Ord)` | arity then varies by given, which `Calli` cannot express (decision C). It also works for exactly the case that does not need it — the ground direct call — and fails for the polymorphic one |
| **Keep the record per given and pass dependencies at each call site.** The caller resolves `Eq[Symbol]` and pushes it | the caller of a `Calli` does not know which given it is dispatching to, so it cannot know what to push. Same objection as above, one level out |

## Hazards

- **Every given member's arity changes**, including the derived ones the binder
  synthesizes. Derivation builds bodies that call other givens' members
  directly, so each of those calls grows an argument — which means derivation
  needs a way to name a record at bind time, not just at emit time. This is the
  largest single piece of the change and the one most likely to move the
  self-hosting count on its own.
- **`Array[any]` slots hold two kinds of thing.** A token is an `int` and a
  dependency is a `Value.Ref`; nothing checks which is which. Reading a
  dependency slot as a token would `Calli` on a heap address. The layout is the
  only thing keeping them apart, so both sides must derive `m` the same way —
  from `traitMembers`, which is already the shared source for the token half.
- **Records multiply.** One per ground goal rather than one per given. The
  compiler's own sources will want `Eq[List[Symbol]]`, `Eq[List[string]]`,
  `Eq[Option[Symbol]]` and so on, each a static field on the program object.
  This is bounded by what the code actually asks for, but it is no longer
  bounded by the number of givens.
- **Interning has to be deterministic.** Stage 3 compares bytecode from two
  compilers, so the record order — and therefore the field order and the
  `$runtimeInit` instruction order — has to be a function of the source, not of
  dictionary iteration.

## Implementation order

Each step should leave the tests green.

1. **`Array[any]` with a dependency half.** Records stay one per given, and
   every given has zero premises today, so `k` is 0 and this is a no-op with a
   changed type token.
2. **Rekey records to the ground goal.** Intern each `Evidence` node during
   resolution; emit in interning order. Still one record per given in practice.
3. **`$ev$self` on every given member**, passed by every caller: the `Calli`
   path, the ground direct-call path, and the bodies derivation synthesizes.
   Needs a bound node for "the record proving *goal*".
4. **Resolve a premise to a dependency slot.** The gap closes here: the test is
   a conditional given whose body uses its premise, bound *and* run.
5. **`[derive(…)]` on a generic type.** `registerDerivation` reports
   `reportDeriveOnGenericType` today; a generic type's derived given is exactly
   a conditional one. This is the step that closes the 62.

Steps 1–4 close what ADR 0004 and ADR 0005 both record as not built. Step 5 is
what moves the count, and it is worth keeping separate: the first four change
how evidence works for every given in the program, and the last one only adds
givens.

## Outcome

Four commits, `931a7b3`..`14a8079`. Steps 1 to 4 are built, which closes the gap
[ADR 0004](0004-traits-given-evidence-and-contextual-extensions.md) and
[ADR 0005](0005-evidence-representation.md) both record as still not built:
`given [T: Eqv] => Eqv[Box[T]]` can write `a.value.equals(b.value)` in its own
body, and it runs. 411 tests, up from 409. Self-hosting diagnostics 417 → 415,
which is the expected shape — none of the 415 is a conditional given.

### The decisions held

**A and B together, not separately.** The implementation order put `Array[any]`
first on the grounds that every given has zero premises today, so the dependency
half would be empty and the change a no-op. That was true and it was also
pointless: a conditional given already got a record under the old scheme, and
that record was already wrong — one per given, for a thing that proves something
different at every instantiation. The two landed in one commit.

**Interning turned out to be the in-progress rule.** `resolveEvidence` built an
`Evidence` tree with a `dependencies` field and every caller threw it away; the
tree was describing the interning table. Replacing it with the table got the
recursion guard ADR 0005 asks for and never had, for free — interning the record
before resolving its premises means a goal that reappears underneath itself
finds the record being built. Without it `Eq[Symbol]` needing `Eq[List[Symbol]]`
needing `Eq[Symbol]` would report "too deeply nested" at depth 32.

**C is what forced `BoundEvidence`.** The plan said the record becomes a
trailing parameter on every given member and a premise is read out of a
dependency slot. The second half has no symbol to name, and `EvidenceCall.evidence`
was a `Symbol`. It is now a `BoundEvidence` — `Held` for the parameter or field
ADR 0005 built, `Premise` for a slot of `$ev$self`. That is the shape the
decision implied and did not say.

### Two things the plan did not have

**A direct call needs a bound node for the record.** Decision C says every
caller passes the record. For a `Calli` the caller already holds it, but the two
direct-call paths — a ground operator, and a contextual extension — do not, and
the emitter cannot recover it: it knows the callee is a given's member but not
which instantiation's record that member was reached through. A
`BoundExpression.EvidenceRecord` carries the static field from binding.

**`findGivenMember` had to start resolving.** It matched a given and returned
the member, never touching the record. Under decision B a record only exists if
something resolved its goal, and a ground call site is exactly such a use — so
the ground path now resolves as well as matches, and hands the record back
alongside the member.

### Step 5

`[derive(Eq)] class Box[T](value: T)` registers `Eq[Box[$0]]` given `Eq[$0]`,
one premise per type parameter in declaration order. 417 tests, up from 411;
self-hosting diagnostics 415 → 407.

**The derived body needed no new resolution rule.** A parameter's goal is
either ground — a concrete type, proved by a given, called directly with its
record, exactly as before — or it mentions the given's own type variables, in
which case it is answered by what the member already holds. Typing `$ev$self`
as *what the given proves* rather than as the record's own `Array[any]` is what
made that fall out: the ordinary evidence walk then finds `$ev$self` for a
parameter of the derived type itself, and `findPremiseMember` finds a premise
for a parameter that is a bare type variable. Only the call node differs —
`EvidenceCall` where a ground goal builds a `Call`.

**A conditional given's member is stated in that given's type variables**, and
three call sites were checking operands against it without instantiating: the
operator path, the contextual-extension path, and the member lookup behind it.
`findGivenMember` now returns the type arguments that matched alongside the
member and the record, and each site substitutes. Without this `a == b` on a
`Box[int]` silently fell through to ADR 0003's reference identity, which
compiles and answers wrongly.

**The contextual-extension path had to start unifying.** It matched a given's
head structurally, which can only ever see a non-generic given — `Show[Box[$0]]`
does not equal `Show[Box<int>]`. It now scans the givens only to discover which
trait might supply the name and hands the goal to `findGivenMember`, so one
place decides which given wins and one place interns the record.

### Still not built

- **A parameter whose type is a composite over a type variable.**
  `class Box[T](held: Holder[T])` reports `no Eq[Holder<$0>] for field held`.
  The goal is neither ground — so there is no static record to point at — nor a
  premise, and the dependency half is sized by the declared context bounds,
  which are one per type parameter. Carrying it would mean the dependency list
  growing as bodies are built, and a fixpoint pass to intern what that adds.
  This is what `List[T]`, `Dictionary[K, V]`, `NonEmptyList[T]`,
  `SeparatedSyntaxList[T]` and `Namespaced[A]` each hit.
- **Generic enums.** `registerEnumDerivation` still rejects them, because
  `matchType` unifies `Type.Class` and falls through to equality on
  `Type.Alias`, which is what an enum's type is. `List` and `Option` are both
  generic enums, so this and the item above are what stand between here and the
  bulk of §1.3a.
- **The orphan rule, named exceptions, and associated members**, all still
  deferred by ADR 0004.
