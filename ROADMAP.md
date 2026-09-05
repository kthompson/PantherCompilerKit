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

Green: 336 tests across the lexer, parser, binder, type checker, VM, metadata
format, and args parser.

### The self-hosted compiler does not

```bash
sbt pnc/compile
```

Runs to completion and reports **195 diagnostics** against the generated
`.pn` sources. By message:

| Count | Diagnostic                      |
| ----: | ------------------------------- |
|    57 | `No operator for operands`      |
|    52 | `Cannot convert from A to B`    |
|    39 | `Type X not defined`            |
|    18 | `Invalid namespace`             |
|    17 | `Symbol X not found for type T` |
|     5 | argument-count mismatches       |
|     5 | `Symbol X not found`            |
|     2 | `Duplicate definition`          |

**2 of the 195 mention an unsolved type variable** (`$0`, `$1`, …) — a
generic parameter the binder gave up on. Name resolution is essentially
done: the five remaining bare `Symbol X not found` are all `File` and `Path`
from `using system.io`. What is left is operators and conversions.

By file:

| Count | File                        |
| ----: | --------------------------- |
|    26 | `Parser.pn`                 |
|    25 | `TypeInference.pn`          |
|    17 | `ExprBinder.pn`             |
|    15 | `Emitter.pn`                |
|    14 | `Lowered.pn`                |
|    13 | `Binder.pn`                 |
|    11 | `VM.pn`                     |
|    10 | `LoweredAssemblyPrinter.pn` |

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

**201 blocks across 22 files: 199 compile, 0 fail, 2 skipped.** The two skips
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

### 1.1 Generated sources stay in step with the transpiler

Every `.pn` file under `pnc/src/` is written by `sbt pncs/transpile`, and
nothing else belongs there. Verify with a transpile followed by a check that
no file predates the run:

```bash
sbt pncs/transpile && find pnc/src -name '*.pn' -not -newermt '-5 minutes'
```

The `transpile` CI job stages with `git add -A` before diffing, so a newly
generated file, a modified one, and a deleted one all fail the build. A bare
`git diff --exit-code` misses untracked files, which is how a stale tracked
name and its regenerated replacement can both sit in the tree.

The transpile itself exits non-zero when any source fails to parse. A Scala
parameter named `to` — a Panther keyword — is enough to stop it, and the tree
keeps whatever was generated last, so treat a parse error here as a failed
build, not a warning.

Two duplicate-definition diagnostics remain, and neither is drift:
`inferCall` in `ExprBinder.pn` and `printNew` in `LoweredAssemblyPrinter.pn`
are overloaded in the Scala source, and Panther has no overload resolution.
Either give them distinct names or add overloading — tracked under §1.3.

A caution for anyone re-checking this on macOS: the default filesystem is
case-insensitive, so a tracked `ast.pn` and a generated `Ast.pn` collapse into
one entry locally while remaining two separate files on Linux CI. Duplicates
of that shape are invisible in a local build.

### 1.2 The exit code reflects the diagnostics

`Program.run` returns the number of diagnostics it reported, and `main` turns
a non-zero count into `exit(1)`
([`Program.scala`](pncs/src/main/scala/Program.scala)). An unparseable
argument list exits non-zero too; `--help` and a clean compile exit zero.

`exit(code: int): never` is a predef intrinsic, defined for the Scala compiler
in [`panther.scala`](runtime/src/main/scala/panther.scala) and registered as a
builtin in [`Binder.scala`](pncs/src/main/scala/Binder.scala) so the
transpiled `Program.pn` binds it. Like `println` and `panic`, it is marked
`extern` and has no emitter or VM support yet — see §3.2.

Because `build.sbt` fails the task on a non-zero exit code
([`build.sbt:146`](build.sbt:146)), `sbt pnc/compile` now **fails** rather
than reporting diagnostics and succeeding. `ProgramTests` covers the
count that decision is made from.

### 1.3 Burn down the diagnostics

Ordered by what the counts say, not by what is interesting:

1. **`string + T` for non-string `T`** — 46, and the largest single item. Every
   remaining `+` diagnostic. Blocked on a decision, not on work: either the
   operator gains an overload against `any`, or the transpiler inserts the
   `string(…)` call the docs already teach. See §4.2.
2. **`Type X not defined`** — 39, and mostly transpiler work rather than binder
   work. Three groups: 20 are an enum case used unqualified as a type
   (`case expr: Match =>` where the case is `Expression.Match`), which is the
   cleanup commit `adc9723` started and did not finish; 13 are Scala type names
   the transpiler leaves alone — `Unit` (10), `String` (2), `Boolean` — where
   Panther spells them `unit`, `string`, `bool`; 6 are `HashMap`, which has no
   Panther equivalent and needs one or a rewrite onto `Dictionary`.
3. **`if`/`else` does not form a union** — 23 of the 52 `Cannot convert` are a
   case against a sibling case of the same enum: `Option.None` to
   `Option.Some<T>`, `MetadataFlags.None` to `MetadataFlags.Static`,
   `LoweredStatement.Goto` to `LoweredStatement.LabelDeclaration`. The else
   branch is checked against the then branch's type; only `match` produces a
   union.
4. **Namespace and import resolution** — 18 `Invalid namespace`, plus the
   five `File`/`Path` from `using system.io`, which exists only in the Scala
   runtime. The transpiler turns `import ns._` into `using ns`; the binder's
   handling of that has a known gap
   ([`Binder.scala:773`](pncs/src/main/scala/Binder.scala:773)).
5. **Member lookup** — 17 `Symbol X not found for type T`: 12 are string and
   int builtins (`substring`, `compareTo`, `nonEmpty`, `endsWith`) that have
   no VM support, and the rest are lookups on a case type, which never
   consults its enum.
6. **String indexing types as `string`, not `char`** — 10 `No operator` and 6
   `Cannot convert`. `str(0)` falls through `ExprBinder.inferCall` to the
   `string(…)` conversion method, because only `Array` has an indexing path
   and everything else lands on the constructor lookup. So `str(0) == '-'`
   compares a string against a char. Needs an indexing path for `string` in
   the binder and a way to read a character out of a `Value.String` in the VM.
7. **Generic inference** (see §2). 2 diagnostics reference an unsolved type
   variable and 11 a parameter that defaulted to `any`.

The one `No operator` left over is `char + int`, at
[`Hex.pn:39`](pnc/src/Hex.pn:39).

`this` is bound as of
[ADR 0002](docs/architecture/adr/0002-binding-this.md), which removed all 35
`Symbol this not found`. It also uncovered three defects in code that was
previously unreachable, none of which show up as diagnostics: an implicit
field read pushes no receiver, a class with no template gets no constructor
body, and enum methods do not emit.

`==` and `!=` bind on reference types as of
[ADR 0003](docs/architecture/adr/0003-equality-on-reference-types.md), which
removed 80 of the 85 equality diagnostics: a value compares against a case of
its own enum, or against another value of its own type, when one side widens
to the other. String ordering landed with it. What that equality *means* at
runtime is not settled and the bytecode is wrong today — `a == b` on two
distinct instances evaluates to `true`, because the instance never reaches the
stack as a `Value.Ref`. That is the ADR 0002 constructor hole, now observable
through the operator; the choice between reference and structural equality is
back-end work under §1.4 and §3.

Track the number after every change:

```bash
sbt pnc/compile
```

**195 → 0.** Nothing else in this section matters until that number moves.

Only the first 20 diagnostics are printed. To see them all, transpile first —
`pnc/compile` does this implicitly, and the count depends on it — then run the
compiler directly with a higher limit:

```bash
sbt pncs/transpile && sbt --error "pncs/run --diagnostics-limit 5000 out.pnb $(find pnc/src -name '*.pn' | tr '\n' ' ')"
```

### 1.4 Close the unimplemented holes

38 `panic("unimplemented: …")` calls remain in paths the self-hosted
compiler will eventually walk: 9 in `Emitter.scala`, 8 in `Lowered.scala`, 4
across the assembly printers, 3 in `Transpiler.scala`, and the rest
scattered. Each names the function it sits in, so `grep unimplemented` finds
them all. Each is a crash waiting for the first program that hits it. They
can stay until the diagnostics are down, but they cannot stay through stage
2.

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
`TypeInference.scala` infers type arguments at call sites, `Types.substitute`
instantiates them through every type constructor, and `TypeTests` covers
identity functions, generic containers, enum aliases, and generic methods with
concrete returns. (`Inference.scala` is commented out in its entirety and is
not part of the pipeline.) What is missing is everything past the simple cases.

[ADR 0001](docs/architecture/adr/0001-generic-type-inference.md) traces every
diagnostic that mentioned a type variable to one of five causes and fixes them
in five measured steps, all in: one substitution, inference over every type
constructor, patterns carrying the scrutinee type, constructors inferring like
calls, and variance-aware conversion.

### 2.1 Type-argument inference through call chains

Type arguments flow through the paths the compiler's own sources use:

```
head match {
  case List.Cons(x, tail) => f(x)
}
```

`x` takes the scrutinee's type argument, `List.Cons(x, xs)` and
`new Dictionary[K, V](List.Nil)` instantiate every parameter, an expected type
solves what the arguments cannot, and an unsolved covariant parameter defaults
to `never` so `Result.Error(e)` satisfies any `Result[E, B]`.

Two diagnostics still mention a type variable and 11 a parameter that
defaulted to `any`. The next lever is the positional-id constraint recorded in
ADR 0001: a generic method on a generic class shares `$0` with its class.

### 2.2 Upper bounds

`GenericTypeParameter` carries an `upperBound: Option[Type]`, and nothing ever
populates it. The parser drops bounds on the floor
([`Parser.scala:395`](pncs/src/main/scala/Parser.scala:395), `// TODO: bounds
support`) and the binder hardcodes `None`
([`Binder.scala:1527`](pncs/src/main/scala/Binder.scala:1527)). `T <: Base`
parses into a `GenericParameterSyntax` with no bounds and is silently ignored.

Wire it end to end: parse, bind, and enforce at the call site with a real
diagnostic.

### 2.3 Variance keywords mean what they say

`out` is covariance and `in` is contravariance, matching C# and Kotlin
([`Variance.scala`](pncs/src/main/scala/Variance.scala)). The transpiler emits
`out` for Scala's `+` and `in` for `-`
([`Transpiler.scala:210`](pncs/src/main/scala/Transpiler.scala:210)), and the
binder reads them back the same way
([`Binder.scala:1505`](pncs/src/main/scala/Binder.scala:1505)), so the
generated stdlib reads `enum List[out T]` for a covariant list.

`BinderTests` pins the mapping directly — a hand-written `[out T]` binds to
`Covariant`, `[in T]` to `Contravariant`. Keep that test: the two sides are
inverses of each other, so flipping both at once is silent in every build
except one that checks a hand-written annotation.

The annotations are recorded but not enforced; that is §2.4.

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

### 4.2 Failures come back as diagnostics

The `diagnostics-not-exceptions` invariant in
[`primitives.yaml`](docs/architecture/primitives.yaml) holds.
`"text " + someInt` reports
`No operator '+' for operands string and int` at the operator's location, and
`break` / `continue` report `break is not supported` rather than taking the
compiler down. `BinderTests` covers all three; a regression to a panic fails
the test as an exception.

The binary-operator case was not a missing check but a comparison that could
never be true: `Type.Error` carries a message, so `resultType == Type.Error`
compared a type against the case constructor. Two more comparisons had the
same shape, in `isSubtype` and `typesWithError`; all three now pattern-match
through an `isErrorType` helper. Lowering was already skipped when the
diagnostic bag is non-empty, so reporting the error was the whole fix — the
panic came from the lowerer walking an error node nothing had reported.

Reporting these errors is what made a parser precedence bug visible: `==`
shared the assignment branch in `currentPrecedence()`, so it bound looser than
`||` and `&&` and `a == b || c == d` grouped as `a == (b || c) == d`. `==` now
sits at its own precedence and `ParserTests` pins the ordering against `||`,
`&&`, and the relational operators.

`==` between an enum type and one of its cases is decided and implemented —
[ADR 0003](docs/architecture/adr/0003-equality-on-reference-types.md). The
other shape, `string + T` for non-string `T`, is not. The docs teach
`"text " + string(n)` and `BinderTests` pins the implicit form as a
diagnostic, but 46 sites in the transpiled sources write it. Either the
operator gains an overload against `any`, or the transpiler inserts the
`string(…)` call. Until that is settled it is the largest single item in §1.3.

`break`/`continue` are rejected, not implemented — they parse, and the
diagnostic is a placeholder for lowering them to jumps.

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

- **Assigning through member access is unimplemented in the lowerer.**
  `obj.field = x` binds, then hits a `???` in `lowerAssignment`
  ([`Lowered.scala`](pncs/src/main/scala/Lowered.scala)), so it takes the
  compiler down for any assignable field. One of the §1.4 holes, and the
  reason `val` enforcement can only be tested end to end on the rejecting
  side.

- **`doccheck --stage bind` lowers.** `Stage.Bind` calls
  `MakeCompilation.create`, which lowers whenever the diagnostic bag is empty
  ([`BlockChecker.scala`](tools/doccheck/src/main/scala/BlockChecker.scala)).
  A snippet that binds cleanly and then hits a lowering `???` crashes the
  checker instead of being reported — the §4.2 pattern, one stage later. A
  bind-only entry point would fix this and give tests a way to check binder
  behaviour independent of lowering.
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
- **`break` and `continue` are rejected, not implemented.** Both are real
  keywords and parse fine, but there is no lowering for them, so the binder
  reports `break is not supported` and stops. Implementing them means loop
  context in the binder and jumps in the lowerer.
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
- **Test coverage is stage-shaped, not feature-shaped.** 336 tests, but
  `MetadataTests` has 2 and there is no end-to-end test that takes source all
  the way to output. §3.4 is the fix.

---

## Suggested order

Sequenced so each step makes the next one measurable.

**First — stop flying blind.** Done. The generated tree matches the
transpiler (§1.1), the exit code is trustworthy (§1.2), and failures come back
as diagnostics rather than exceptions (§4.2). The 195 counts every error the
front end finds — none are discarded.

**Second — generics.** This was the plan, and the measurement has overtaken it.
Only 13 of the 195 are generics: 2 mention a type variable, 11 a parameter that
defaulted to `any`. §2.1 and §2.2 are still worth doing, but they cannot make
the number fall sharply, because the number is not made of generics.

What it is made of, in order: `string + T` (46), `Type X not defined` (39), the
sibling-case conversions an `if`/`else` cannot union (23), `Invalid namespace`
(18), member lookup on builtins and case types (17), string indexing (16).
Those are §1.3 items 1–6 — ordinary front-end and transpiler work, not type
theory. Take them before §2.

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

| Metric                            |         Now | Target | Command                                     |
| --------------------------------- | ----------: | -----: | ------------------------------------------- |
| Self-hosting diagnostics          |         195 |      0 | `sbt pnc/compile` (now fails, as it should)  |
| Doc blocks that fail              | **0 / 201** |      0 | `sbt "doccheck/run docs/src/content/docs"`  |
| Doc blocks skipped as unsupported |           2 |      0 | as above                                    |
| Samples that run in CI            |           0 |      6 | not yet built                               |
