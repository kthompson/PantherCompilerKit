# Panther Roadmap

Where the compiler kit is going, and what has to be true before it gets there.

The three things that matter most:

1. **Self-hosting** — `pnc` compiles itself without help from Scala. 10
   diagnostics stand between here and there, all itemized below.
2. **Generics** — inference is done (§2.1); upper bounds, variance
   enforcement, and generic aliases/unions are not (§2.2–§2.5).
3. **Sample programs** — a program can be written, compiled, and run. 4 of 6
   are written; the other 2 wait on a standard-library decision.

Every number below is measured, not estimated, and every command shown
reproduces the measurement. Re-run them rather than trusting the number.

---

## Where things stand

### The Scala compiler works

```bash
sbt pncs/compile && sbt test/test
```

Green: 523 tests across the lexer, parser, binder, type checker, VM, metadata
format, transpiler, and args parser.

### The self-hosted compiler is 10 diagnostics from clean

```bash
sbt pnc/compile
```

| Count | Diagnostic                      |
| ----: | -------------------------------- |
|     4 | `Cannot derive Eq`/`Show`        |
|     3 | argument-count mismatches        |
|     2 | `Duplicate definition`           |
|     1 | `Cannot convert from A to B`     |

All ten trace back to one thing, tracked in full at §1.3: Panther has no
overload resolution, and `inferCall` (`ExprBinder.scala`) and `printNew`
(`LoweredAssemblyPrinter.scala`) are each declared twice with different
signatures in the Scala source. Every name in the generated tree resolves,
every operator has a match, and no diagnostic mentions an unsolved type
variable — this is the whole list.

`sbt pnc/compile` fails the build on this count (`build.sbt:146`), by design
(§1.2). **It is not run in CI** — only `pncs/compile` and `test/test` are —
so a regression here would not be caught automatically. Worth fixing before
relying on the count.

### The docs compile

```bash
sbt "doccheck/run docs/src/content/docs"
```

**201 blocks across 22 files: 199 compile, 0 fail, 2 skipped.** The two skips
are on `higher-order-functions.md`, which documents lambdas and function-typed
parameters — neither exists (see [cross-cutting](#cross-cutting)).

The tool lives in [`tools/doccheck/`](tools/doccheck/README.md) and is as
useful for finding compiler holes as for finding prose errors — most of the
language gaps under cross-cutting surfaced through it.

### Programs compile, run, and round-trip through an image

`pncs --run source.pn` compiles and executes in one step. `pncs out.pnb
source.pn` writes a bytecode image, and `ImageRunner.exec` loads one and runs
it — a program can be compiled once and run later, and what comes back out of
an image behaves the same as what went in. What's left is the `pvm` command
itself (§3.2) — loading and running an image is a function with tests, not
yet a command line.

### There are 4 sample programs, and a stdlib decision blocks the rest

`samples/hello.pn`, `fizzbuzz.pn`, `fib.pn`, and `expr.pn` each run and are
pinned four ways — output, symbols, lowered assembly, disassembly — by
`SampleTests`, which runs as part of `sbt test/test` and so is already in CI.
`wordcount.pn` and `records.pn` are blocked on the same thing that blocks
doccheck's biggest gap (§4.4): `Option`, `List`, `Dictionary`, and `Result`
live in `pnc/src`, the transpiled compiler, not anywhere a user program can
reach. `val x = Option.Some(1)` in a sample reports `Symbol Option not found`.

---

## 1. Self-hosting

The goal: `pnc` compiles `pnc` and the output is byte-identical to the input
compiler's output.

### 1.1 Generated sources stay in step with the transpiler

Every `.pn` file under `pnc/src/` is written by `sbt pncs/transpile`, and
nothing else belongs there. CI's `transpile` job stages with `git add -A`
before diffing, so a newly generated file, a modified one, and a deleted one
all fail the build. A bare `git diff --exit-code` misses untracked files,
which is how a stale tracked name and its regenerated replacement can both sit
in the tree.

A caution for anyone re-checking this on macOS: the default filesystem is
case-insensitive, so a tracked `ast.pn` and a generated `Ast.pn` collapse into
one entry locally while remaining two separate files on Linux CI.

### 1.2 The exit code reflects the diagnostics

`Program.run` returns a `RunResult` — `Diagnostics(count)` or `Executed(code)`
— and `main` turns a non-zero diagnostic count into `exit(1)`
([`Program.scala`](pncs/src/main/scala/Program.scala)). Because `build.sbt`
fails the task on a non-zero exit code
([`build.sbt:146`](build.sbt:146)), `sbt pnc/compile` **fails** rather than
reporting diagnostics and succeeding — but see the note above: nothing in CI
runs it.

### 1.3 Burn down the diagnostics

1. **Overloads** — the 2 `Duplicate definition` (`inferCall`, `printNew`),
   the 2 argument-count mismatches that are `inferCall(node, scope)` called
   against its three-parameter overload, and the 1 `Cannot convert`, which is
   `printNew(left)` picking the `LoweredExpression.New` overload for a
   `LoweredLeftHandSide.New`. Panther has no overload resolution; either
   rename the two pairs to distinct names or add it.
2. **A stray argument count** — `println()` with no argument, in
   `compilation.pn:154`, which Scala allows (an overload) and Panther does
   not.
3. **4 derivation reports that should stay** — `Cannot derive Eq`/`Show` for
   `ConversionClassifier` (a field of `ExprBinder`) and `AstPrinter` (a field
   of `SymbolPrinter`). Both types genuinely have no meaningful equality or
   string form. The fix is at the two call sites — give them a hand-written
   instance, or stop deriving there — not in derivation itself. See the
   fourth pass of [ADR 0004](docs/architecture/adr/0004-traits-given-evidence-and-contextual-extensions.md)
   for why a reference-identity fallback would be wrong.

Track the number after every change:

```bash
sbt pnc/compile
```

To see the full list instead of just the first 20:

```bash
sbt pncs/transpile && sbt --error "pncs/run --diagnostics-limit 5000 out.pnb $(find pnc/src -name '*.pn' | tr '\n' ' ')"
```

Everything else that used to sit in this section — `Type X not defined`,
`Invalid namespace`, `Symbol X not found` (including member lookup on a case
type), `No operator`, and the branch-widening `Cannot convert`s from `if`/`else`
and generic call sites — is at zero. `this` binds
([ADR 0002](docs/architecture/adr/0002-binding-this.md)), and `==`/`!=` bind
on reference types ([ADR 0003](docs/architecture/adr/0003-equality-on-reference-types.md)).
That second one left a real runtime gap, not a diagnostic: **the bytecode is
wrong**. `a == b` on two distinct instances evaluates to `true`, because the
instance never reaches the stack as a `Value.Ref` — the ADR 0002 constructor
hole, observable through the operator. Deciding reference vs. structural
equality and fixing the codegen is back-end work, tracked at §1.4/§3.

### 1.3b Make derivation cheaper on the stack

Not blocking anything — the default stack is 8192 slots, about 680 plain
frames or a derived `Eq` over a 500-element list — but still open. A derived
`Eq` over a recursive type costs `20 + 16n` stack slots for a list of `n`
elements, against `15 + 12n` for the equivalent hand-written recursive
function. Three things worth trying, in order of expected payoff:

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

### 1.4 Close the unimplemented holes

36 `panic("unimplemented: …")` calls remain in paths the self-hosted compiler
will eventually walk: 12 in `ExprBinder.scala`, 10 in `Lowered.scala`, 8 in
`Emitter.scala`, 3 in `Transpiler.scala`, 2 in `Binder.scala`, 1 in
`LoweredAssemblyPrinter.scala`. `grep unimplemented` finds them all. Each is a
crash waiting for the first program that hits it — they can stay until the
diagnostics are down, but not through stage 2.

Five of the ten in `lowerAssignment` alone
([`Lowered.scala:396`](pncs/src/main/scala/Lowered.scala:396)) are
unimplemented: assigning through `ArrayCreation`, `Call`, `EvidenceCall`,
`MemberAccess`, and `New` left-hand-sides all panic. Only `Index` and a plain
`Variable` are lowered. `obj.field = x` binds, then hits the `MemberAccess`
case — the reason `val` enforcement can only be tested end to end on the
rejecting side.

### 1.5 Run the stages

`scripts/stage0.ps1` (transpile), `stage1.ps1` (bootstrap), `stage2.ps1`
(`pnc/compile`) already exist. Once §1.3 and §1.4 are clear, add **stage 3**:
`pnc`-compiled-by-`pnc` must produce the same bytecode as
`pnc`-compiled-by-`pncs`. That fixed point is what "self-hosted" means, and it
is the point at which `pncs` stops being load-bearing.

---

## 2. Generics

`Type.Class`, `Type.GenericClass`, `Type.GenericFunction`, and `Type.Variable`
all exist, `TypeInference.scala` infers type arguments at call sites,
`Types.substitute` instantiates them through every type constructor, and
`TypeTests` covers identity functions, generic containers, enum aliases, and
generic methods with concrete returns. (`Inference.scala` is commented out in
its entirety and is not part of the pipeline.)

### 2.1 Type-argument inference through call chains — done

[ADR 0001](docs/architecture/adr/0001-generic-type-inference.md) traced every
diagnostic that mentioned a type variable to one of five causes; all five are
fixed, and no diagnostic mentions an unsolved type variable any more. The
constraint worth remembering for what's next: a generic method on a generic
class shares its positional type-variable ids (`$0`, `$1`, …) with the class,
which is why a call's type arguments have to be resolved all-or-nothing
against the callee rather than incrementally.

### 2.2 Upper bounds

`GenericTypeParameter` carries an `upperBound: Option[Type]`, and nothing ever
populates it. The parser drops bounds on the floor
([`Parser.scala:395`](pncs/src/main/scala/Parser.scala:395), `// TODO: bounds
support`) and the binder hardcodes `None`
([`Binder.scala:1527`](pncs/src/main/scala/Binder.scala:1527)). `T <: Base`
parses into a `GenericParameterSyntax` with no bounds and is silently ignored.

Wire it end to end: parse, bind, and enforce at the call site with a real
diagnostic.

### 2.3 Variance keywords — done

`out`/`in` (covariance/contravariance, matching C# and Kotlin) parse,
transpile, and bind correctly end to end
([`Variance.scala`](pncs/src/main/scala/Variance.scala),
[`Transpiler.scala:210`](pncs/src/main/scala/Transpiler.scala:210),
[`Binder.scala:1505`](pncs/src/main/scala/Binder.scala:1505)), and
`BinderTests` pins the mapping directly. They're recorded but not enforced —
that's §2.4.

### 2.4 Variance enforcement

`Variance` is recorded on every type parameter and never checked. A covariant
parameter in an input position should be a diagnostic. Needed before the
variance annotations in the stdlib mean anything.

### 2.5 Generic type aliases and unions

`Type.Alias` takes type arguments and `Types.union` carries a `TODO` about
redundant cases. Both are load-bearing for the pattern-matching code in the
binder. Lower priority than 2.2, higher than 2.4.

---

## 3. Sample programs

### 3.1 Write and read `.pnb` — done

`pncs out.pnb source.pn` writes an image (magic, version, entry token, chunk,
metadata tables, all as big-endian ints); `ImageRunner.exec` loads one and
runs it. `Compilation.emit(output)` honours its argument, which it used to
take and ignore.

### 3.2 Add a runner — mostly done

`pncs --run source.pn` compiles and executes in one step, and the prelude
intrinsics (`println`, `print`, `panic`, `exit`, `assert`, `mod`, `File`/`Path`)
all run through one `callx <builtin id>` instruction (§3.2a). **What's left:
`pvm output.pnb`** — load and execute a compiled image from the command
line — which is now pure wiring over `ImageRunner`, unblocked by §3.1.

### 3.2a One instruction for every builtin — done

[`Builtin`](metadata/src/main/scala/Builtin.scala) maps a method's qualified
name to an id; the VM's `nativeCall` runs it. Declaring a builtin is a binder
declaration, a table entry, and a VM case — the instruction set itself
doesn't grow. The one thing `callx` doesn't cover is the two extern *fields*,
`Array.length` and `string.length`, matched by identity in
`emitMemberAccess` and emitting `Ldlen` instead.

### 3.3 Write the remaining samples

| Sample         | Exercises                                        | Status |
| -------------- | ------------------------------------------------- | ------ |
| `hello.pn`     | top-level statements, `println`                   | done   |
| `fizzbuzz.pn`  | `while`, `if`/`else`, `mod`, string building       | done   |
| `fib.pn`       | recursion, `int` arithmetic                        | done   |
| `expr.pn`      | `enum`, pattern matching, recursion over a tree    | done   |
| `wordcount.pn` | `Array`, `List`, `Dictionary`, string handling      | blocked on stdlib-in-scope |
| `records.pn`   | classes, fields, generics                          | blocked on stdlib-in-scope |

The blocker is real and worth deciding rather than routing around: either a
stdlib the driver compiles alongside user sources, or samples restricted to
what the prelude actually provides. It's the same decision §4.4 needs for
doccheck.

`SampleTests` snapshots each sample four ways (`.out`, `.symbols`, `.lowered`,
`.disasm`); regenerate with `UPDATE_SNAPSHOTS=1 sbt "test/testOnly SampleTests"`
and read the diff before accepting it. `TranspileSnapshotTests` separately
pins the Scala-to-Panther direction over `test/fixtures/transpile/`.

### 3.4 Samples in CI — effectively done

`SampleTests` runs as part of `sbt test/test`, which is already a CI job — so
every sample that exists is compiled, run, and diff-checked against a
snapshot on every PR. Nothing further needed here once §3.3 writes the last
two.

---

## 4. Documentation that is checked

[`tools/doccheck/`](tools/doccheck/README.md) extracts every ` ```panther `
block from the markdown and runs it through the real front end. Blocks can be
annotated in place:

```markdown
<!-- panther-check: parse-only -->
<!-- panther-check: expect-error -->
<!-- panther-check: skip reason="..." -->
```

### 4.1 Keep the baseline empty

`tools/doccheck/baseline.txt` is picked up automatically and is **empty**, so
CI enforces a clean run. If an entry ever appears, shorten the list again
rather than living with it: `sbt "doccheck/run --update-baseline docs/src/content/docs"`.

Two blocks stay `skip`, both on `higher-order-functions.md`, until lambdas
land (see [cross-cutting](#cross-cutting)).

### 4.2 Failures come back as diagnostics — the invariant holds

The `diagnostics-not-exceptions` invariant in
[`primitives.yaml`](docs/architecture/primitives.yaml) holds, pinned by
`BinderTests`: a bad binary operator, `break`/`continue`, and an implicit
`string + T` all report a diagnostic rather than taking the compiler down.
`==` between an enum type and one of its cases is decided
([ADR 0003](docs/architecture/adr/0003-equality-on-reference-types.md)).
`string + T` is decided too: 47 call sites in the transpiled sources now write
`string(x)` explicitly rather than the operator gaining an overload against
`any` — matching what the docs teach and what Scala was doing implicitly
(`string(x)` is `x.toString`). The implicit form stays a diagnostic so it
can't drift back. `break`/`continue` are rejected, not implemented — they
parse, and the diagnostic is a placeholder for lowering them to jumps.

### 4.3 Clean up the docs tree

- `docs/src/content/docs/guides/` duplicates `basics/`, `data-types/`,
  `flow-control/`, and `functions/`. Pick one and delete the other; every fix
  currently has to be made in two places to stay consistent.
- `guides/example.md` and `reference/example.md` are unedited Starlight
  boilerplate.
- 17 of the 22 doc files are CRLF, against the LF rule in `.editorconfig`.
- `docs_old/` (mdbook) and the empty `tools/mdbook/` can go once nothing is
  being ported out of them.

### 4.4 Grow the checker

In rough order of usefulness:

- **Execute blocks and check their output.** Docs are full of
  `// Prints: 0, 1, 2, 3, 4`. No longer blocked — §3.1/§3.2 both exist now.
- **Linked blocks**, so a walkthrough can build up across several fences
  instead of needing `parse-only`.
- **A stdlib prelude — the most limiting gap.** `Option`, `Result`, `Either`,
  `List`, and `Dictionary` live in `pnc/src/*.pn` and are _not_ builtins: only
  `int`, `string`, `bool`, `char`, `unit`, and `Array` are wired into the
  binder. A single-block compilation can't mention `Option` at all. Compiling
  each block alongside the runtime `.pn` sources would fix this and would
  match what a real Panther program sees — the same decision blocking §3.3's
  remaining samples.
- **An opt-in prelude for page-local types**, so a snippet can use `User`
  without declaring it.
- **Point it at `README.md` and `CONTRIBUTING.md`** too.

---

## Cross-cutting

Things that don't belong to one goal but block several. None of these have
moved.

- **No `float`/`double`, and decimal literals do not even lex.** `99.99`
  comes back as `Unexpected token NumberToken, expected IdentifierToken` —
  the lexer reads `99`, `.`, `99` as a member access.
- **No compound assignment.** `count += 5` does not parse; `+` and `=` are
  lexed separately.
- **No `return`.** Not a keyword anywhere in the lexer or
  `SyntaxFacts.getKeywordKind`; `return x` binds as an unknown identifier.
  Functions are trailing-expression only.
- **Block comments do not nest.** `/* outer /* inner */ more */` ends at the
  first `*/`.
- **`break` and `continue` are rejected, not implemented.** Real keywords,
  parse fine, no lowering — the binder reports `break is not supported` and
  stops. Needs loop context in the binder and jumps in the lowerer.
- **No pattern guards, alternation, or tuples.** `case n if n > 0 =>` does not
  parse, neither does `case 1 | 2 =>`, and there's no tuple literal or tuple
  pattern — only class/enum extraction patterns like `Point(x, y)`.
- **Arrays cannot resize.** Four `// TODO: support resizing` in
  `Parser.scala`. The parser works around it with fixed-size arrays and
  rebuilds.
- **No lambdas.** `Type.Function` exists and the binder understands function
  types, but `(x) => x + 1` does not parse. The only feature the docs
  document as absent (`functions/higher-order-functions.md`), and the reason
  for the two `skip` blocks in §4.1.
- **No lexer support for exponents, shifts, or hex/binary literals.**
  `0x504e4200` lexes as `0` followed by the identifier `x504e4200`. Found
  writing the `.pnb` magic (§3.1), which is spelled in decimal because of it
  — exactly where a program wants hex.
- **The `==`/`!=` runtime bug** at §1.3: reference equality on distinct
  instances evaluates `true` today.
- **Test coverage is stage-shaped, not feature-shaped** outside of
  `SampleTests` — 523 tests, but most pin one stage against a hand-written
  expectation rather than source-to-output.

---

## What's left, roughly in order

1. **§1.3** — resolve the `inferCall`/`printNew` overload pair (rename or add
   overload resolution) and the stray `println()` in `compilation.pn`. That's
   6 of the 10 diagnostics; the other 4 (derivation) need a call-site fix, not
   a diagnostics-count fix.
2. **Gate `sbt pnc/compile` in CI.** It isn't run there today, so the count
   above isn't actually protected from regressing.
3. **§1.4** — close the 36 `unimplemented` panics, starting with
   `lowerAssignment`'s five.
4. **§1.5** — stage 3, the self-hosting fixed point.
5. **Decide the stdlib-in-scope question** (§3.3/§4.4) — it's the one thing
   blocking the last 2 samples and doccheck's biggest gap simultaneously.
6. **§3.3** — write `wordcount.pn` and `records.pn` once #5 is decided; §3.4
   needs nothing further.
7. **§3.2** — wire up `pvm`.
8. **§2.2, §2.4, §2.5** — upper bounds, variance enforcement, generic aliases.
9. **§4.3** — docs tree cleanup.
10. Cross-cutting language gaps, as they block something above (lambdas block
    two doc pages; hex literals and `return` are small and self-contained).

## Tracking

| Metric                            |         Now | Target | Command                                     |
| --------------------------------- | ----------: | -----: | -------------------------------------------- |
| Self-hosting diagnostics          |          10 |      0 | `sbt pnc/compile` (fails on non-zero; not yet run in CI) |
| — of those, derivation             |           4 |      0 | §1.3 item 3                                  |
| Doc blocks that fail              | **0 / 201** |      0 | `sbt "doccheck/run docs/src/content/docs"`   |
| Doc blocks skipped as unsupported |           2 |      0 | as above                                     |
| Samples written / passing in CI   |         4/6 |    6/6 | `sbt "test/testOnly SampleTests"`            |
