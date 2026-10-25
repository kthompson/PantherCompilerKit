# Panther Roadmap

What is left to do. Finished work is not listed here; see the git history,
the [ADRs](docs/architecture/adr/), and
[ADR 0007](docs/architecture/adr/0007-garbage-collection.md#implementation-outcome-2026-10-10)
for the self-hosting measurements.

The compiler self-hosts: `pnc` compiles itself to a byte-identical image, and
CI requires it (`sbt pnc/compile stage3/run`). The priorities now:

1. **Remove the Scala dependency** — no JVM, sbt, or Scala source (§1).
2. **Generics** — upper bounds, variance enforcement, and generic
   aliases/unions (§2).
3. **Programs** — a standalone `pvm` command and the last two samples (§3).

---

## 1. Remove the Scala dependency

[docs/architecture/scala-exit.md](docs/architecture/scala-exit.md) lays out
the phases: PVM spec, host-API builtins, native `pvm`, seed image, making
`pnc/src` canonical, test and tool ports, then deletion.

Phase 1's spec is written ([pvm.md](docs/architecture/pvm.md)). Its §9 lists
twelve reference-VM behaviours (D1–D12) that still have to be fixed in Scala
or adopted into the spec. Phase 2 (host-API builtins) can start now.

---

## 2. Generics

### 2.1 Upper bounds

`GenericTypeParameter` carries an `upperBound: Option[Type]`, but the binder
always populates it with `None`. Subtype upper bounds (`T <: Base`) need parser,
binder, and call-site enforcement with a real diagnostic.

Trait context bounds (`[T: Eq]`) are separate and already work.

### 2.2 Variance enforcement

`out`/`in` parse and are recorded on every type parameter, but never checked.
A covariant parameter in an input position should be a diagnostic. Needed
before the variance annotations in the stdlib mean anything. Lower priority
than §2.3.

### 2.3 Generic type aliases and unions

`Type.Alias` takes type arguments and `Types.union` carries a `TODO` about
redundant cases. Both are load-bearing for the pattern-matching code in the
binder.

A constraint worth remembering for all three: a generic method on a generic
class shares its positional type-variable ids (`$0`, `$1`, …) with the class,
so a call's type arguments have to be resolved all-or-nothing against the
callee ([ADR 0001](docs/architecture/adr/0001-generic-type-inference.md)).

---

## 3. Programs

### 3.1 A `pvm` command

`pvm output.pnb [args…]` — load and execute a compiled image from the command
line. On the JVM this is wiring over `ImageRunner.execWithArgs`, and it is the
first step of [scala-exit.md Phase 3](docs/architecture/scala-exit.md#phase-3--native-pvm).
The exit-code mapping is in [pvm.md §3.4](docs/architecture/pvm.md#34-termination).

### 3.2 The remaining samples

| Sample         | Exercises                                      |
| -------------- | ---------------------------------------------- |
| `wordcount.pn` | `Array`, `List`, `Dictionary`, string handling |
| `records.pn`   | classes, fields, generics                      |

Both can use the bundled stdlib. Add each to `SampleTests` and generate its
snapshots with `UPDATE_SNAPSHOTS=1 sbt "test/testOnly SampleTests"`, reading
the diff before accepting it.

---

## 4. Documentation

### 4.1 Grow the checker

In rough order of usefulness:

- **Convert `// Prints:` comments to `output` fences.** Blocks with an
  `output` fence are already run and checked; the docs still carry many
  unchecked `// Prints:` comments.
- **Linked blocks**, so a walkthrough can build up across several fences
  instead of needing `parse-only`. A `panther-context` comment covers shared
  declarations but not shared statements.
- **Point it at `README.md` and `CONTRIBUTING.md`** too.

Two blocks on `functions/higher-order-functions.md` stay `skip` until lambdas
land.

---

## 5. Derivation stack cost

Not blocking anything — the default stack is 8192 slots, about 680 plain
frames or a derived `Eq` over a 500-element list. A derived `Eq` over a
recursive type costs `20 + 16n` stack slots for a list of `n` elements,
against `15 + 12n` for the equivalent hand-written recursive function. In
order of expected payoff:

1. **Five locals for a body that is a chain of `&&` over field comparisons.**
   Lowering spills more temporaries than the shape needs; not specific to
   derivation.
2. **The evidence record is loaded twice per call**, once as the `$ev$self`
   argument and once to read the token off of. `emitEvidenceCall` could `Dup`
   instead.
3. **`Eq` on a list is not naturally recursive.** `derivedEqChain` recurses
   because the type is recursive, but the comparison is a fold; a derived
   member that looped instead would be flat in `n`. The real fix, and the
   largest change. `Eq[Array[T]]` in the prelude already does this by hand.

---

## Cross-cutting

Language gaps and bugs that affect more than one goal. After the Scala exit
these hurt more, since the compiler is then maintained in Panther by hand.

- **`&&` and `||` do not short-circuit.** The emitter lowers them to `and`/`or`,
  so `false && f()` calls `f()`
  ([pvm.md §6.3](docs/architecture/pvm.md#63-control)).
- **A literal receiver crashes the binder.** `"b".compareTo("a")` and
  `42.describe()` panic with `bindLHS called with non-LHS expression`; binding
  the literal to a `val` first works
  ([#83](https://github.com/kthompson/PantherCompilerKit/issues/83)).
- **No `float`/`double`, and decimal literals do not even lex.** `99.99`
  comes back as `Unexpected token NumberToken, expected IdentifierToken` —
  the lexer reads `99`, `.`, `99` as a member access.
- **No compound assignment.** `count += 5` does not parse; `+` and `=` are
  lexed separately.
- **No `return`.** Not a keyword anywhere in the lexer or
  `SyntaxFacts.getKeywordKind`; `return x` binds as an unknown identifier.
  Functions are trailing-expression only.
- **`break` and `continue` are rejected, not implemented.** Real keywords,
  parse fine, no lowering — the binder reports `break is not supported` and
  stops. Needs loop context in the binder and jumps in the lowerer.
- **Block comments do not nest.** `/* outer /* inner */ more */` ends at the
  first `*/`.
- **No pattern guards, alternation, or tuples.** `case n if n > 0 =>` does not
  parse, neither does `case 1 | 2 =>`, and there's no tuple literal or tuple
  pattern — only class/enum extraction patterns like `Point(x, y)`.
- **Arrays cannot resize.** Four `// TODO: support resizing` in
  `Parser.scala`. The parser works around it with fixed-size arrays and
  rebuilds.
- **No lambdas.** `Type.Function` exists and the binder understands function
  types, but `(x) => x + 1` does not parse. The reason for the two `skip`
  blocks in §4.1.
- **No exponent, hex, or binary literals.** `0x504e4200` lexes as `0`
  followed by the identifier `x504e4200`, which is why the `.pnb` magic is
  spelled in decimal.
- **Test coverage is stage-shaped, not feature-shaped** outside of
  `SampleTests` — most tests pin one stage against a hand-written expectation
  rather than source-to-output. scala-exit.md Phase 6 proposes the fix.

---

## What's next, roughly in order

1. **Settle pvm.md's D1–D12** and **add the host-API builtins**
   (scala-exit.md Phases 1–2).
2. **Wire up the `pvm` command** (§3.1).
3. **Write `wordcount.pn` and `records.pn`** (§3.2).
4. **Generics** (§2.1, §2.3, then §2.2).
5. **Extend doccheck** (§4.1).
6. Fix cross-cutting gaps as they block the work above.

## Tracking

| Metric | Now | Target | Command |
| --- | --- | --- | --- |
| PVM spec defects settled | 0 / 12 | 12 | [pvm.md §9](docs/architecture/pvm.md#9-defects-to-settle) |
| Samples written | 4 / 6 | 6 | `sbt "test/testOnly SampleTests"` |
| Doc blocks skipped or ignored as unsupported | 9 | 0 | `sbt "doccheck/run docs/language docs/architecture"` |
| Scala source lines | ~25k in `pncs` alone | 0 | `git ls-files '*.scala' \| xargs wc -l` |

CI also gates on what is already done: 0 self-hosting diagnostics, the Stage 3
fixed point, every test passing, and 0 failing doc blocks.
