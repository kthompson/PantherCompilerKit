# ADR 0003: Equality on reference types

**Status:** Accepted — implemented 2026-09-04, see [Outcome](#outcome)
**Date:** 2026-09-04
**Primitives:** `parser` (the operator table lives there), `binder`, `vm-runtime`
(see [`primitives.yaml`](../primitives.yaml))
**Roadmap:** [§1.3](../../../ROADMAP.md#13-burn-down-the-diagnostics)

## Context

`No operator` is the largest category in the self-hosting diagnostics: 138 of
284. 85 of those are `==` or `!=`, and every one is a comparison the operator
table cannot express.

### The table only knows the value types

[`BinaryOperators.scala`](../../../pncs/src/main/scala/BinaryOperators.scala)
holds a flat list of `BinaryOperator(left, right, operator, result)` rows,
populated in the constructor for `int`, `bool`, `char` and `string`.
`checkBinary` walks the list looking for an exact `left == right == operator`
match and returns `Type.Error` when it finds none.

That is workable for the value types, which are fixed and few. It cannot work
for anything the program declares: there is no row for `SymbolKind`, and there
never can be, because `SymbolKind` is written in the source being compiled.

### What the compiler's own sources compare

Grouped by shape, from the 85:

| Shape                          | Count | Example                      |
| ------------------------------ | ----: | ---------------------------- |
| A value against a related case |    58 | `kind == SymbolKind.Field`   |
| Two values of the same type    |    22 | `resultType == expectedType` |
| A `string` against a `char`    |     5 | `str(0) == '-'`              |

The first two are the ones to fix. The third is not an equality gap at all —
see [string indexing](#string-indexing-is-a-separate-defect) below.

### The conversion classifier already knows the answer

[`ConversionClassifier`](../../../pncs/src/main/scala/Conversion.scala) already
computes exactly the relation these comparisons need. `classify(from, to)`
returns `Identity` for a case converting to its own enum — the comment on
`classifyTypeArguments` says so directly — and `widensTo` wraps it as the
no-cast-required predicate. `SymbolKind.Field` widens to `SymbolKind`;
`Option.None` widens to `Option[string]` through the generic-alias path; a type
widens to itself.

Nothing new has to be computed. The table just needs a fallback that asks.

### The back end already emits it

`Emitter.emitBinaryExpression` maps `Equals` to `Opcode.Ceq` and `NotEquals` to
`Ceq` followed by a negation, with no reference to operand types. Lowering
carries a `BoundExpression.Binary` through to `LoweredExpression.BinaryExpression`
the same way. So binding these comparisons adds no new emitter or lowerer case.

Whether the bytecode is *correct* is a different question, taken up under
[Consequences](#consequences).

## Decision

Add one fallback to `checkBinary`, reached only when the table has no row.

### The rule

For `==` and `!=`, when neither operand is a value type, the comparison binds to
`bool` if either operand widens to the other:

```
referenceEqualityApplies(left, right) =
  !isValueType(left) && !isValueType(right) &&
  (widensTo(left, right) || widensTo(right, left))
```

Directionality is what makes the enum cases work. `SymbolKind.Field` widens to
`SymbolKind` but not the reverse, and `kind == SymbolKind.Field` is written both
ways round in the sources, so one direction succeeding is enough.

### Value types are excluded, deliberately

The exclusion is not an optimisation. Without it, `char == int` would start
binding through the `char`-to-`int` implicit conversion, and `string == char`
would still fail but for an accidental reason rather than a stated one. The
value types have a complete table; what it rejects there is a real mismatch, and
this ADR does not reopen it.

### Unrelated types stay a diagnostic

`Box == Bag` for two unrelated classes has no conversion in either direction and
continues to report. This is the property that makes the rule a rule rather than
"reference types compare equal to anything".

## Consequences

**The runtime semantics are not settled, and the bytecode is wrong today.**
This is the cost of the change and it should be stated plainly: comparisons that
used to be compile errors now compile and produce a wrong answer at runtime.

Measured, on a class with a template so that it gets a constructor body at all:

```panther
class Box(v: int) { def get(): int = v }
val a = new Box(1)
val b = new Box(2)
a == b        // evaluates to true
```

`a` on its own evaluates to `Value.Int(0)`. The instance never reaches the stack
as a `Value.Ref`, so `Ceq` compares two zeros. The defect is upstream of
equality — it is the constructor and field-emission hole ADR 0002 recorded — but
equality is now a way to observe it.

Beyond that bug there is a genuine open question. `pnc` is generated from the
Scala sources, so `==` in Panther has to mean what `==` means in Scala for the
self-hosted compiler to behave like the one it was transpiled from. In Scala
that is structural equality for a `case class` and reference equality for a
plain `class`. Panther has one kind of class, and its extraction patterns work
on all of them, so structural is the consistent reading — and structural
equality on `Type` or `Symbol` is a deep recursive walk that no opcode performs.
`Ceq` on two `Value.Ref`s cannot answer it, and `VM.toInt` panics on a `Ref`
rather than trying.

Deciding that, and implementing it, is back-end work tracked under §1.4 and §3.
Binding is a prerequisite for it either way: nothing downstream can be built or
tested while the front end rejects the expression.

**Nullary enum cases are unreachable as values.** `Color.Red` in expression
position hits `emitMemberAccess: unsupported symbol kind Class`, the hole ADR
0002 found. So the single most common shape in the sources —
`kind == SymbolKind.Field` — binds but cannot yet run, for a reason that has
nothing to do with the operator.

**A new diagnostic became visible.** `Emitter.emitClassMetadata` now reports
`Cannot convert from MetadataFlags.None to MetadataFlags.Static`. Its
`symbol.kind == SymbolKind.Object` used to be an error expression, which
suppressed everything downstream of it; now that the comparison binds, the
`if`/`else` beneath it is checked and hits the union gap that is already item 2
on the burn-down list. Same pattern as ADR 0002: a fix upstream exposes a defect
that was hiding behind an error node.

**`checkBinary` is no longer a pure table lookup.** It consults the binder's
conversion classifier, which means the operator table now depends on the type
system rather than only on itself. `BinaryOperators` already takes the binder as
a constructor argument and only reads `classifier` at binding time, well after
`Binder` has constructed it, so there is no initialisation-order exposure.

## String ordering

Landed alongside, because it is the same table and the same one-line shape.

`<`, `<=`, `>` and `>=` had no rows for `string`, so `VM.scala`'s own
lexicographic comparison — `aStr > bStr`, at
[`VM.scala:189`](../../../pncs/src/main/scala/VM.scala:189) — did not compile
under the self-hosted compiler. The VM implements `Clt` and `Cgt` on
`Value.String` directly and the emitter builds `<=` and `>=` out of those two,
so all four already worked end to end and only the front end was missing.
`VmTests` now covers all four against string literals.

## Alternatives considered

**Add `==` and `!=` rows for every declared type as it is bound.** Keeps
`checkBinary` a pure lookup. Rejected: the table would grow with the program, the
rows would have to be added at exactly the right point in binding, and the
enum-case rows would be quadratic in the number of cases. The relation is
already computed; storing it is worse than asking for it.

**Special-case `==` in `inferBinary` instead of in the table.** Equivalent in
effect. Rejected because it splits operator resolution across two files, and
`checkBinary` is what `BoundAssemblyPrinter` and any future overload resolution
would have to agree with.

**Require a subtype relation rather than a conversion.** `isSubtype` is the more
obvious predicate, but it does not know about enum cases: it handles functions,
arrays, and identity, and returns false for `SymbolKind.Field` against
`SymbolKind`. Teaching it the enum relation would duplicate what the classifier
already does.

**Allow `==` between any two reference types.** Simpler, and it would clear the
same 80 diagnostics. Rejected: it also accepts `Box == Bag`, which is always
false and always a bug, and the diagnostic for it is worth keeping.

**Defer until the runtime semantics are decided.** Rejected. The decision needs
`Type` and `Symbol` comparisons to be expressible before it can be tested at all,
and holding 80 diagnostics hostage to a back-end design question stalls §1.3
behind §3.

## Outcome

Two steps, measured separately.

| Step                        | Diagnostics |
| --------------------------- | ----------: |
| baseline                    |         284 |
| equality on reference types |         205 |
| string ordering             |         204 |
| **total**                   |    **−80**  |

80 of the 85 `==`/`!=` diagnostics are gone — every shape the rule covers — and
one new one appeared, the `MetadataFlags` conversion described above. The 5 that
remain are the `string`-against-`char` group, which is a different defect. The
string-ordering rows cleared one more. 284 tests pass, up from 278. `doccheck`
stays at 199 of 201, 0 failing.

`No operator` falls from 138 to 57, and it is no longer the largest category —
`Cannot convert` is, at 52. Every one of the 57 that remain involves `string` or
`char` on one side:

| Count | Shape                                        |
| ----: | -------------------------------------------- |
|    46 | `string + T` for non-string `T`              |
|    10 | `string` against `char`                      |
|     1 | `char + int`                                 |

Which reduces the category to two open questions, both already named in
[§4.2](../../../ROADMAP.md#42-failures-come-back-as-diagnostics):

### `string + T` is still undecided

The docs teach `"text " + string(n)` and `BinderTests` pins `"text " + n` as a
diagnostic. 46 sites in the transpiled sources write the implicit form. Either
the operator gains an overload against `any`, or the transpiler inserts the
`string(…)` call. This ADR does not decide it.

### String indexing is a separate defect

The 10 `string`-against-`char` diagnostics are not an operator gap. `str(0)`
where `str: string` reaches the class-call path in `ExprBinder.inferCall`, which
only special-cases `Array`; everything else falls through to the constructor
lookup, and for `string` that finds the `string(…)` conversion method. So `str(0)`
types as `string` rather than `char`, and `str(0) == '-'` reports an operator
that is missing for the right reason and the wrong types.

Fixing it means an indexing path for `string` in the binder returning `char`,
and a way to read a character out of a `Value.String` in the VM. Worth its own
entry; it is not equality's problem.
