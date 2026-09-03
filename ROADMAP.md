# Panther Roadmap

Where the compiler kit is going, and what has to be true before it gets there.

The three things that matter most:

1. **Self-hosting** — `pnc` compiles itself without help from Scala.
2. **Generics** — the type system holds up under the code the compiler is
   already written in.
3. **Sample programs** — someone can write, compile, and _run_ a Panther
   program that does something.

They are not independent. Generic inference is the single largest thing
standing between `pnc` and self-hosting, and there is currently no way to run a
compiled program at all, which caps how convincing any sample can be.

Every number below is measured, not estimated, and every command shown
reproduces the measurement. Re-run them rather than trusting the number.

---

## Where things stand

### The Scala compiler works

```bash
sbt pncs/compile && sbt test/test
```

Green: 246 tests across the lexer, parser, binder, type checker, VM, metadata
format, and args parser.

### The self-hosted compiler does not

```bash
sbt pnc/compile
```

Runs to completion and reports **1058 diagnostics** against the generated
`.pn` sources. (1029 of those come from the committed tree; the other 29 come
from `Ast.pn`, which the transpile step writes — see §1.1.) By message:

| Count | Diagnostic                      |
| ----: | ------------------------------- |
|   642 | `Cannot convert from A to B`    |
|   181 | `Symbol X not found for type T` |
|   130 | `Symbol X not found`            |
|    38 | `Type X not defined`            |
|    32 | `Duplicate definition`          |
|    18 | `Invalid namespace`             |
|    16 | argument-count mismatches       |
|     1 | no operator for operands        |

**685 of the 1058 mention an unsolved type variable** (`$0`, `$1`, …) — a
generic parameter the binder gave up on. That is the strongest single signal we
have about what to fix first.

By file, the damage concentrates in the parts of the compiler that lean hardest
on generic collections:

| Count | File               |
| ----: | ------------------ |
|   245 | `Binder.pn`        |
|   118 | `ExprBinder.pn`    |
|    83 | `Lowered.pn`       |
|    80 | `Parser.pn`        |
|    72 | `Emitter.pn`       |
|    54 | `TypeInference.pn` |

### Nothing is ever written to disk

`Compilation.emit(output)` takes an output path
([`compilation.scala:97`](pncs/src/main/scala/compilation.scala:97)) and
ignores it. `Emitter.emit()` builds a chunk and metadata tables in memory and
returns them ([`Emitter.scala:156`](pncs/src/main/scala/Emitter.scala:156));
the only code in the repo that writes files is the transpiler.

So `pncs output.pnb source.pn` — the invocation in the README, in
`ArgsParser.printUsage()`, and in the docs — produces no `output.pnb`. There is
no `.pnb` reader either, and no CLI path to `Compilation.exec()`, even though
the VM works and `VmTests` drives it in-process for 36 tests.

### The docs compile

```bash
sbt "doccheck/run docs/src/content/docs"
```

**200 blocks across 22 files: 198 compile, 0 fail, 2 skipped.** The two skips
are on `higher-order-functions.md`, which documents lambdas and function-typed
parameters — neither exists (see cross-cutting below).

The tool that enforces this lives in
[`tools/doccheck/`](tools/doccheck/README.md), and it is as useful for finding
compiler holes as for finding prose errors: most of the language gaps listed
under [cross-cutting](#cross-cutting) surfaced through it.

The syntax it holds every snippet to:

| Construct         | Panther                          |
| ----------------- | -------------------------------- |
| Iteration         | `for (i <- 0 to n)`, `while`     |
| Generic types     | `Array[int]`, `Option[User]`     |
| Primitives        | `int`, `string`, `bool`, `char`  |
| Enum cases        | `Option.Some(x)`, `Option.None`  |
| Functions         | `def f(): T = x`                 |
| Pattern matching  | `x match { case 1 => ... }`      |
| String formatting | `"text " + string(n)`            |

### There are no sample programs

There is not one `.pn` file in the repository outside `pnc/src/`, which is
generated. Nothing shows what a Panther program looks like end to end, and
nothing would run if it did.

---

## 1. Self-hosting

The goal: `pnc` compiles `pnc` and the output is byte-identical to the input
compiler's output.

### 1.1 Fix the generated-source drift — small, do it first

`sbt pncs/transpile` emits `Ast.pn` and `AstPrinter.pn`. `pnc/src/ast.pn` and
`pnc/src/printer.pn` are committed alongside them and define the same types.
All four get compiled, which accounts for the 32 duplicate-definition
diagnostics on its own.

CI cannot see this. The `transpile` job runs `git diff --exit-code`,
which ignores **untracked** files, so `Ast.pn` appearing as a brand-new
generated file passes the check silently.

- Delete `pnc/src/ast.pn` and `pnc/src/printer.pn`.
- Change the CI check to `git status --porcelain` (or `git diff --exit-code`
  after `git add -A`) so new and deleted generated files fail the build too.

### 1.2 Make the compiler exit non-zero on errors

`Program.run` prints `found N diagnostics` and returns normally
([`Program.scala:94`](pncs/src/main/scala/Program.scala:94)). `sbt pnc/compile`
"succeeds" with 1058 errors. Nothing downstream — CI, scripts, an editor
integration — can tell success from failure.

### 1.3 Burn down the diagnostics

Ordered by what the counts say, not by what is interesting:

1. **Generic inference** (see §2). 685 diagnostics reference an unsolved type
   variable; this is the bulk of the work.
2. **Member lookup on generic receivers** — `Symbol X not found for type $0`,
   181 diagnostics. Falls out of §2 but worth tracking separately in case it
   does not.
3. **Namespace and import resolution** — 18 `Invalid namespace`, plus some
   share of the 130 bare `Symbol not found`. The transpiler turns
   `import ns._` into `using ns`; the binder's handling of that has a known
   gap ([`Binder.scala:773`](pncs/src/main/scala/Binder.scala:773)).

Track the number after every change:

```bash
sbt pnc/compile
```

**1058 → 0.** Nothing else in this section matters until that number moves.

Only the first 20 diagnostics are printed. To see them all, transpile first —
`pnc/compile` does this implicitly, and the count depends on it — then run the
compiler directly with a higher limit:

```bash
sbt pncs/transpile && sbt --error "pncs/run --diagnostics-limit 5000 out.pnb $(find pnc/src -name '*.pn' | tr '\n' ' ')"
```

### 1.4 Close the `???` holes

38 live `???` remain in paths the self-hosted compiler will eventually walk:
9 in `Emitter.scala`, 8 in `Lowered.scala`, 4 across the assembly printers,
3 in `Transpiler.scala`, and the rest scattered. Each is a crash waiting for
the first program that hits it. They can stay until the diagnostics are down,
but they cannot stay through stage 2.

### 1.5 Run the stages

`scripts/stage0.ps1` (transpile), `stage1.ps1` (bootstrap), `stage2.ps1`
(`pnc/compile`) already exist. Once stage 2 is clean, add **stage 3**:
`pnc`-compiled-by-`pnc` must produce the same bytecode as
`pnc`-compiled-by-`pncs`. That fixed point is what "self-hosted" means, and it
is the point at which `pncs` stops being load-bearing.

---

## 2. Generics

Generics are further along than the diagnostic count suggests — `Type.Class`,
`Type.GenericClass`, `Type.GenericFunction`, and `Type.Variable` all exist,
`Inference.scala` and `TypeInference.scala` are real implementations, and
`TypeTests` covers identity functions, generic containers, and generic methods
with concrete returns. What is missing is everything past the simple cases.

### 2.1 Type-argument inference through call chains

The failing pattern in `pnc/src` is not exotic. It is code like:

```
head match {
  case List.Cons(x, tail) => f(x)
}
```

where `x`'s type has to flow from the scrutinee's type argument into the call.
The binder produces `$0` and then reports `Cannot convert from $0 to T`.

This is the highest-value work in the entire roadmap. Every diagnostic it
removes from §1.3 is one that does not need fixing by hand.

### 2.2 Upper bounds

`GenericTypeParameter` carries an `upperBound: Option[Type]`, and nothing ever
populates it. The parser drops bounds on the floor
([`Parser.scala:395`](pncs/src/main/scala/Parser.scala:395), `// TODO: bounds
support`) and the binder hardcodes `None`
([`Binder.scala:1527`](pncs/src/main/scala/Binder.scala:1527)). `T <: Base`
parses into a `GenericParameterSyntax` with no bounds and is silently ignored.

Wire it end to end: parse, bind, and enforce at the call site with a real
diagnostic.

### 2.3 Fix the inverted variance keywords

[`Variance.scala`](pncs/src/main/scala/Variance.scala) documents
`Covariant // "out"` and `Contravariant // "in"`, matching C# and Kotlin. Both
places that touch the keywords disagree with it:

- [`Transpiler.scala:210`](pncs/src/main/scala/Transpiler.scala:210) emits
  `in` for Scala's `+` (covariant) and `out` for `-`.
- [`Binder.scala:1505`](pncs/src/main/scala/Binder.scala:1505) reads `"in"`
  back as `Covariant` and `"out"` as `Contravariant`.

The two inversions cancel, so nothing is broken today — which is exactly why
this will stay wrong until someone writes `[out T]` by hand and gets
contravariance. The generated stdlib currently reads `enum List[in T]` for a
covariant list. Fix both sides, regenerate, and add a test that a hand-written
`[out T]` binds to `Covariant`.

### 2.4 Variance enforcement

`Variance` is recorded on every type parameter and, as far as the binder is
concerned, never checked. A covariant parameter in an input position should be
a diagnostic. Needed before the variance annotations in the stdlib mean
anything.

### 2.5 Generic type aliases and unions

`Type.Alias` takes type arguments and `Types.union` carries a `TODO` about
redundant cases. Both are load-bearing for the pattern-matching code in the
binder. Lower priority than 2.1, higher than 2.4.

---

## 3. Sample programs

Blocked on output. A sample that cannot be run is a code listing.

### 3.1 Write and read `.pnb`

The metadata tables already have the shape for this — `BlobTable.write`,
`SignatureBuilder`, `IntList` — but there is no whole-image writer and no
reader. Needed:

- A serializer for `EmitResult` (chunk + metadata) to a `.pnb` file.
- A deserializer that reconstitutes it.
- A round-trip test: emit, write, read, execute, compare against
  `Compilation.exec()` on the same source.

Once this exists, `Compilation.emit(output)` can honour its argument and the
README stops documenting a command that does nothing.

### 3.2 Add a runner

`Compilation.exec()` already runs the VM in-process and `VmTests` leans on it.
Expose it:

- `pncs --run source.pn` — compile and execute in one step.
- `pvm output.pnb` — load and execute a compiled image. This is the command in
  the Getting Started docs; today it does not exist.

### 3.3 Then write the samples

In a new `samples/` directory, each with a comment header saying what it
demonstrates and what it prints:

| Sample             | Exercises                                       |
| ------------------ | ----------------------------------------------- |
| `hello.pn`         | top-level statements, `println`                 |
| `fizzbuzz.pn`      | `while`, `if`/`else`, `mod`, string building    |
| `fib.pn`           | recursion, `int` arithmetic                     |
| `wordcount.pn`     | `Array`, `List`, `Dictionary`, string handling  |
| `expr.pn`          | `enum`, pattern matching, recursion over a tree |
| `option-result.pn` | `Option` and `Result` as error handling         |

`expr.pn` is the important one: a small expression evaluator is the shape of a
compiler, so it exercises the same features `pnc` needs and doubles as a
regression test for §1.

### 3.4 Put the samples in CI

Every sample compiles, runs, and produces expected output. A `samples` job that
diffs actual output against a checked-in `.expected` file. This is the first
end-to-end test the project would have.

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

`tools/doccheck/baseline.txt` is picked up automatically and is **empty**, so
CI enforces a clean run: any snippet that stops compiling fails the build.

### 4.1 Keep the baseline empty

Nothing is baselined. If an entry appears, shorten the list again rather than
living with it:

```bash
sbt "doccheck/run --update-baseline docs/src/content/docs"
```

Two blocks are marked `skip`, both on `higher-order-functions.md`, because
lambdas and function-typed parameters do not exist. That page documents the
gap and the workarounds available today — named top-level functions, generic
functions, and `enum` + `match` for behaviour dispatch. The skips come off when
lambdas land (see [cross-cutting](#cross-cutting)).

### 4.2 The compiler throws on `string + int`

`"text " + someInt` raises
`Binary operator 'Plus' not found for types 'string' and 'int'` as an
exception rather than a diagnostic, violating the `diagnostics-not-exceptions`
invariant in [`primitives.yaml`](docs/architecture/primitives.yaml). The docs
sidestep it with `"text " + string(n)`, the idiom the compiler's own sources
use.

Two things to settle: whether `string + int` should work at all, and,
regardless of that, that the failure comes back as a diagnostic. The same
applies to `break`/`continue`, which panic through the same
`boundErrorStatement` path.

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
  `// Prints: 0, 1, 2, 3, 4`. Depends on §3.1/§3.2.
- **Linked blocks**, so a walkthrough can build up across several fences
  instead of needing `parse-only`.
- **A stdlib prelude — the most limiting gap.** `Option`, `Result`, `Either`,
  `List`, and `Dictionary` live in `pnc/src/*.pn` and are _not_ builtins: only
  `int`, `string`, `bool`, `char`, `unit`, and `Array` are wired into the
  binder. A single-block compilation therefore cannot mention `Option` at all
  (`Type Option not defined`), which means the checker currently forbids
  documenting a large part of the standard library. Compiling each block
  alongside the runtime `.pn` sources would fix this, and would match what a
  real Panther program sees. Until then the tool is shaping the docs, which is
  backwards.
- **An opt-in prelude for page-local types**, so a snippet can use `User`
  without declaring it.
- **Point it at `README.md` and `CONTRIBUTING.md`** too — the README's
  `pncs output.pnb source.pn` is exactly the kind of claim this catches.

---

## Cross-cutting

Things that do not belong to one goal but block several.

- **`val` is not enforced.** `val x = 10` followed by `x = 20` compiles with
  zero diagnostics. The binder records the distinction and never checks it, so
  the immutability the docs promise does not exist. Cheap to fix and worth
  doing early — every day it stays, more code depends on it.
- **No `float`/`double`, and decimal literals do not even lex.** `99.99` comes
  back as `Unexpected token NumberToken, expected IdentifierToken` — the lexer
  reads `99`, `.`, `99` as a member access. `Math.scala` carries a `TODO` about
  the missing type; the literal syntax is a separate, earlier problem.
- **No compound assignment.** `count += 5` does not parse; `+` and `=` are
  lexed separately.
- **No `return`.** It is not a keyword anywhere in the lexer or
  `SyntaxFacts.getKeywordKind`, so `return x` binds as an unknown identifier.
  Functions are trailing-expression only. Worth an explicit diagnostic rather
  than `Symbol return not found`.
- **Block comments do not nest.** `/* outer /* inner */ more */` ends at the
  first `*/`.
- **`break` and `continue` crash the compiler.** Both are real keywords and
  parse fine, but `bindBreakStatement` and `bindContinueStatement` call
  `boundErrorStatement`, which calls `panic()`
  ([`ExprBinder.scala:695`](pncs/src/main/scala/ExprBinder.scala:695)). Any
  program using them takes the compiler down rather than getting a diagnostic —
  another instance of the §4.2 pattern. Either implement them or reject them
  with a real diagnostic.
- **No pattern guards, alternation, or tuples.** `case n if n > 0 =>` does not
  parse (`parseMatchCase` goes straight from pattern to `=>`), neither does
  `case 1 | 2 =>`, and there is no tuple literal or tuple pattern — only
  class/enum extraction patterns like `Point(x, y)`, which work on any class
  and without `new`.
- **Arrays cannot resize.** Four `// TODO: support resizing` in `Parser.scala`.
  The parser works around it with fixed-size arrays and rebuilds.
- **No lambdas.** `Type.Function` exists and the binder understands function
  types, but `(x) => x + 1` does not parse, so function values cannot be
  written. This is the only feature the docs have to document as absent
  (`functions/higher-order-functions.md`) and the reason for the two `skip`
  blocks in §4.1.
- **No lexer support for exponents or shifts**
  ([`Lexer.scala:243`](pncs/src/main/scala/Lexer.scala:243)).
- **Test coverage is stage-shaped, not feature-shaped.** 246 tests, but
  `MetadataTests` has 2 and there is no end-to-end test that takes source all
  the way to output. §3.4 is the fix.

---

## Suggested order

Sequenced so each step makes the next one measurable.

**First — stop flying blind.**
§1.1 generated-source drift, §1.2 exit codes, §4.2 exceptions where
diagnostics belong. Small, independent, and each one makes a signal
trustworthy that currently is not.

**Second — generics.**
§2.1 inference, §2.2 bounds, §2.3 variance keywords. The 1058 should fall
sharply. If it does not, the assumption behind this roadmap was wrong and the
plan should be rewritten around what the diagnostics actually say.

**Third — make programs runnable.**
§3.1 `.pnb` read/write, §3.2 the runner. Unblocks samples, output-checked docs,
and the stage-3 bootstrap comparison.

**Fourth — samples and the rest of the docs.**
§3.3 the samples, §3.4 samples in CI, §4.3 the duplicated `guides/` tree. The
doc blocks already compile, so nothing here is blocked on §4.1.

**Fifth — finish self-hosting.**
§1.3 to zero, §1.4 the `???` holes, §1.5 stage 3.

## Tracking

The three numbers worth putting on a wall:

| Metric                            |         Now | Target | Command                                    |
| --------------------------------- | ----------: | -----: | ------------------------------------------ |
| Self-hosting diagnostics          |        1058 |      0 | `sbt pnc/compile`                          |
| Doc blocks that fail              | **0 / 200** |      0 | `sbt "doccheck/run docs/src/content/docs"` |
| Doc blocks skipped as unsupported |           2 |      0 | as above                                   |
| Samples that run in CI            |           0 |      6 | not yet built                              |
