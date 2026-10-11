# Panther Roadmap

Where the compiler kit is going, and what has to be true before it gets there.

The three things that matter most:

1. **Self-hosting** — `pnc` compiles itself without help from Scala.
   Diagnostics and the tracked `unimplemented` panics are both at zero.
   The Stage 3 fixed point is verified with garbage collection (§1.5), and
   the CI compiler job now requires it.
2. **Generics** — inference is done (§2.1); upper bounds, variance
   enforcement, and generic aliases/unions are not (§2.2–§2.5).
3. **Sample programs** — a program can be written, compiled, and run. 4 of 6
   are written; the standard library is now available for the remaining 2.

Build, test, doccheck, and Stage 3 results below were verified on 2026-10-10.
The resource measurements are from `98e7026`; the stdlib follow-up also verifies
matching user-program images from both compilers. Re-run the commands when
assessing a newer revision.

---

## Where things stand

### The Scala compiler works

```bash
sbt pncs/compile && sbt test/test
```

Green: 573 tests across the lexer, parser, binder, type checker, VM, metadata
format, transpiler, and args parser.

### The self-hosted compiler builds and compiles itself

```bash
sbt pnc/compile stage3/run
```

`pnc/compile` succeeds with **0 diagnostics** and writes `pnc/target/pnc.pnb`.
It is gated in [CI](.github/workflows/ci.yml), alongside the Scala build,
tests, source regeneration, documentation checks, and formatting.

Stage 3 runs the resulting compiler over its own generated sources and emits
an identical image. The CI compiler job requires this fixed point. See §1.5
for the hash, heap measurements, and resource budget.

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

### There are 4 sample programs, and the stdlib is available

`samples/hello.pn`, `fizzbuzz.pn`, `fib.pn`, and `expr.pn` each run and are
pinned four ways — output, symbols, lowered assembly, disassembly — by
`SampleTests`. `wordcount.pn` and `records.pn` remain to be written.

Both compiler drivers and doccheck include a source-based stdlib automatically:
`Option`, `Result`, `Either`, `List`, `Dictionary`, and `Tuple2`. The default
bundle is embedded in each compiler, independent of the working directory.
`--no-stdlib` supports bootstrap builds and isolated compilation;
`--stdlib <directory>` replaces the bundle. See [the stdlib guide](stdlib/README.md).

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
([`build.sbt`](build.sbt)), `sbt pnc/compile` **fails** rather than
reporting diagnostics and succeeding — and, per the note above, CI now runs
it, so that failure gates the build.

### 1.3 Burn down the diagnostics

1. ~~**Overloads**~~ — done. Panther has no overload resolution, so
   `inferCall` split into `inferCallNode` (the two-argument, AST-based form)
   and `inferCallBound` (the three-argument, already-bound form), and
   `printNew` split into `printNewLeftHandSide` and `printNewExpr`. Fixing the
   `inferCall` shadowing let the checker reach a previously-unreached `panic`
   branch in `inferLHS`, which called `expr.toString()` — not a real method on
   either side, since `[derive(Eq, Show)]` only gives Panther a `show()`, and
   Scala's case-class `toString()` has no Panther counterpart at all. Dropped
   the interpolated expression from that message instead of picking a method
   that would work in one language and not the other.
2. ~~**A stray argument count**~~ — done. `println()` with no argument in
   `compilation.pn:154` became `println("")`, which Scala allows (an
   overload) and Panther does not.
3. ~~**4 derivation reports that should stay**~~ — done. `Cannot derive
   Eq`/`Show` for `ConversionClassifier` (a field of `ExprBinder`) and
   `AstPrinter` (a field of `SymbolPrinter`) — both types genuinely have no
   meaningful equality or string form, per the fourth pass of
   [ADR 0004](docs/architecture/adr/0004-traits-given-evidence-and-contextual-extensions.md).
   The fix was at the two call sites, not in derivation itself:
   `ExprBinder` and `SymbolPrinter` are `case class` in Scala, so the
   transpiler correctly gave them `[derive(Eq, Show)]` — but both are
   stateful services nothing ever compares or prints, exactly like
   `ConversionClassifier` and `AstPrinter` themselves. Changed both to plain
   `class` in `pncs/src/main/scala/{ExprBinder,SymbolPrinter}.scala` and
   regenerated `pnc/src` with `sbt pncs/transpile`, which dropped the
   attribute — the same shape `ConversionClassifier` and `AstPrinter` already
   had. Neither of those two types was touched; deriving reference identity
   for them would have been wrong, which is why a hand-written instance was
   never the right call here.

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
The constructor and reference-value codegen gaps recorded in ADRs 0002/0003
were subsequently closed. Reference identity is the fallback; an `Eq` given
or derived `Eq` supplies structural equality. `VmTests` covers derived
class/enum equality and distinct parameterless enum cases, and the managed
heap gives separate zero-field objects distinct addresses.

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

Done. The 26 tracked `panic("unimplemented: …")` calls are gone from both the
Scala compiler and its generated Panther sources. The closing pass handled
five groups:

- `ExprBinder` now checks array creation against an expected type, treats a
  generic simple name as the symbol it names, reports invalid aliases as
  diagnostics, and rejects function-typed left-hand-side shapes that have no
  direct-call bytecode form as "expression is not callable".
- `Transpiler` rewrites Scala import aliases (`{name => alias}`) to Panther's
  `name as alias` and preserves Panther-style aliases.
- `Binder` turns the impossible "symbol exists but has no registered type"
  state into an internal diagnostic and an error type, so malformed input does
  not take the compiler down.
- `Emitter` emits an early lowered return as its value plus `Ret`. Missing
  metadata tokens and error operators are compiler invariants, so their old
  placeholder panics now identify the exact broken invariant and symbol.
- `LoweredAssemblyPrinter` prints lowered return statements.

Reproduce the count with:

```bash
rg 'panic\("unimplemented:' pncs/src/main/scala pnc/src
```

It prints nothing. `sbt pnc/compile` remains green after regenerating
`pnc/src`.

~~`Lowered.scala`~~ — done, all 10. Two different fixes, depending on whether
the panic was actually reachable:

- `lowerAssignment`'s `ArrayCreation`, `Call`, `EvidenceCall`, and `New`
  cases panicked because the binder let `foo() = x`, `new Foo() = x`, and
  similar non-lvalues bind as valid assignments with no diagnostic —
  `reportIfReadOnly`/`getLHSType` only special-cased `Variable` and
  `MemberAccess`. The fix is in `ExprBinder.scala`, not the lowerer: a new
  `isAssignableLHS` check rejects them at bind time with "expression is not
  assignable", the same diagnostic that already existed
  ([`DiagnosticBag.scala`](pncs/src/main/scala/DiagnosticBag.scala))
  but had no reachable call site. That makes the four `lowerAssignment` cases
  genuinely unreachable, the same as the existing
  `panic("bindLHS called with non-LHS expression")`
  ([`ExprBinder.scala`](pncs/src/main/scala/ExprBinder.scala)).
- `lowerLeftHandSide`'s `ArrayCreation` and `EvidenceCall` cases were real
  gaps: `new Array[int](5).length`, or a trait member called through
  evidence used as the receiver of further member access, both bind cleanly
  and used to crash. Both now lower the same way the adjacent `Call`/`Index`
  cases already did — stash the result in a temporary local.
- `lowerStatement`'s `BoundStatement.Error` case panicked on `break`,
  `continue`, and a duplicate declaration, each of which already reports its
  own diagnostic and returns `Error` specifically so the compiler doesn't go
  down (see the comment on `bindBreakStatement`). Lowering now honors that
  intent and skips the statement instead of panicking — currently
  unreachable in practice, since `MakeCompilation.create` skips lowering
  entirely whenever binding leaves any diagnostic, but correct if that gate
  ever changes.

`obj.field = x` (`MemberAccess`) and array/index assignment were already
lowered before this pass. A `var` field assigned through member access is
now covered end to end by
[`BinderTests.scala`](test/src/test/scala/BinderTests.scala) ("should allow
assigning to a var field through member access") — the positive case the
`val` rejection test used to have no counterpart for.

### 1.5 Run the stages

**Fixed point verified locally on 2026-10-10.**

```bash
sbt pnc/compile stage3/run
```

The PowerShell equivalent is `./scripts/stage3.ps1`. Stage 2 regenerates and
compiles `pnc/src` in canonical source order. The [Stage 3 runner](tools/stage3/)
loads `pnc/target/pnc.pnb`, passes an `Array[string]` command line to it,
writes `pnc/target/pnc-stage3.pnb`, and compares both bytes and SHA-256.

The following resource measurements predate the stdlib bundle (`98e7026`).

| Measurement | Result |
| --- | ---: |
| Stage 2 and Stage 3 image size | 2,475,160 bytes each |
| Managed heap capacity | 33,554,432 slots |
| Execution stack capacity | 2,097,152 slots |
| Collections | 3 |
| Cumulative allocated slots | 77,558,461 |
| Cumulative reclaimed slots | 61,657,902 |
| Peak live slots | 21,252,603 |
| Stage 3 sbt task elapsed time | 101 seconds |
| Stage 2 + Stage 3 warm-build wall time | 109.16 seconds |
| GNU time maximum resident set size | 1,994,120 KiB (about 1.90 GiB) |
| Verifier JVM maximum heap | 4 GiB |
| CI job timeout | 15 minutes |

Matching image SHA-256:

```text
283df1c92d0ca3eedce6174bd6ac104fb20d81b186268cc43c3b306ff3edd65e
```

With the stdlib bundle, Stage 2 and Stage 3 also match: 2,518,316 bytes,
SHA-256 `a0816bc1abaa4ab34d369a227669f3a0b04adc8b348661b4479fd1a7439561e9`.
Stage 3 additionally checks matching stdlib user images, standalone execution,
and self-hosted `--run` with both the embedded and replacement library.

The VM now uses a precise, non-moving mark-and-sweep
[managed heap](pncs/src/main/scala/ManagedHeap.scala), with stack/static/native
roots, coalescing free lists, and size bins. Integer dictionaries, bytecode,
and metadata buffers use paged storage; image output streams those pages
instead of requiring a large contiguous managed array.
[ADR 0007](docs/architecture/adr/0007-garbage-collection.md) records the design
and implementation outcome.

The existing `pnc` CI job now runs `sbt -J-Xmx1G pnc/compile stage3/run`:
1 GiB maximum heap for sbt, 4 GiB for the forked verifier, and a 15-minute
job timeout to allow dependency downloads and cold compilation. This retains
the job name while extending its checks to byte equality.

The resource run used GNU `/usr/bin/time -v` over `sbt pnc/compile stage3/run`
on the local Java 21 host with warm build artifacts. Its maximum resident set
size is the largest process RSS reported by GNU time, not the simultaneous
sum of sbt and its children. The slot counts describe the guest heap. CI uses
Java 17, so its elapsed time and memory may differ. Older non-collecting runs
exhausted 32M–128M slots; those failures no longer describe this implementation.

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

`GenericTypeParameter` carries an `upperBound: Option[Type]`, but the binder
still populates it with `None`. Subtype upper bounds (`T <: Base`) need parser,
binder, and call-site enforcement with a real diagnostic.

This is separate from **trait context bounds**, which already work:
`[T: Eq]` requires evidence for `Eq[T]`. The parser preserves these bounds,
the binder resolves them, and the emitter passes evidence to generic calls.
Conditional givens and `[derive(Eq, Ord, Show)]` are also implemented; see
[ADR 0004](docs/architecture/adr/0004-traits-given-evidence-and-contextual-extensions.md)
and [ADR 0006](docs/architecture/adr/0006-conditional-givens.md).

### 2.3 Variance keywords — done

`out`/`in` (covariance/contravariance, matching C# and Kotlin) parse,
transpile, and bind correctly end to end
([`Variance.scala`](pncs/src/main/scala/Variance.scala),
[`Transpiler.scala`](pncs/src/main/scala/Transpiler.scala),
[`Binder.scala`](pncs/src/main/scala/Binder.scala)), and
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
| `wordcount.pn` | `Array`, `List`, `Dictionary`, string handling      | ready to write |
| `records.pn`   | classes, fields, generics                          | ready to write |

The standard library is now compiled alongside user sources by both compiler
drivers. The remaining samples can use it directly.

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
- **A stdlib prelude — done.** Binding checks now use the same bundled source
  library as user compilations. Documentation uses the actual `Option` and
  `Result` APIs instead of redeclaring them.
- **An opt-in prelude for page-local types**, so a snippet can use `User`
  without declaring it.
- **Point it at `README.md` and `CONTRIBUTING.md`** too.

---

## Cross-cutting

Remaining language and coverage gaps that affect more than one goal.

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
- **No lexer support for exponents or hex/binary literals.**
  `0x504e4200` lexes as `0` followed by the identifier `x504e4200`. Found
  writing the `.pnb` magic (§3.1), which is spelled in decimal because of it
  — exactly where a program wants hex.
- **Test coverage is stage-shaped, not feature-shaped** outside of
  `SampleTests` — 573 tests, but most pin one stage against a hand-written
  expectation rather than source-to-output.

---

## What's left, roughly in order

1. **Write `wordcount.pn` and `records.pn`** (§3.3) using the bundled standard
   library; existing sample tests already run in CI.
2. **Wire up the standalone `pvm` command** (§3.2).
3. **Complete generic upper bounds, aliases/unions, and variance enforcement**
   (§2.2, §2.4, §2.5).
4. **Clean up the docs tree** (§4.3) and extend doccheck (§4.4).
5. Address cross-cutting language gaps as they block the work above.

## Tracking

| Metric | Now | Target | Command |
| --- | --- | --- | --- |
| Self-hosting diagnostics | 0 | 0 | `sbt pnc/compile` (gated in CI) |
| Self-hosting `unimplemented` panics | 0 | 0 | §1.4; `rg 'panic\("unimplemented:' pncs/src/main/scala pnc/src` |
| Stage 3 fixed point | verified; required by CI | maintain | `sbt pnc/compile stage3/run` |
| Tests passing | 573 | all | `sbt test/test` |
| Doc blocks that fail | 0 / 201 | 0 | `sbt "doccheck/run docs/src/content/docs"` |
| Doc blocks skipped as unsupported | 2 | 0 | as above |
| Samples written / passing in CI | 4/6 | 6/6 | `sbt "test/testOnly SampleTests"` |
