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

Green: 451 tests across the lexer, parser, binder, type checker, VM, metadata
format, transpiler, and args parser.

### The self-hosted compiler does not

```bash
sbt pnc/compile
```

Runs to completion and reports **37 diagnostics** against the generated
`.pn` sources. By message:

| Count | Diagnostic                      |
| ----: | ------------------------------- |
|    19 | `Cannot convert from A to B`    |
|     5 | `Symbol X not found`            |
|     4 | `Cannot derive Eq`/`Show`       |
|     4 | argument-count mismatches       |
|     3 | `Symbol X not found for type T` |
|     2 | `Duplicate definition`          |

**4 of the 37 are derivation, and both types should stay unprovable.**
They peaked at 222 when the transpiler started emitting `[derive(Eq, Show)]`.
§1.3a is finished.

**The other 33** were 195 until the `while` fix in §1.3b took six
`Cannot convert` with it, the wildcard-import cleanup took twenty
`Type X not defined`, porting `TypeInference` off `scala.collection.mutable`
took the last six of those along with all 18 `Invalid namespace`, unioning
the branches of an `if` took twenty-one more `Cannot convert`, string
indexing took eighteen across both, writing the string conversions out by
hand took the last 47 `No operator`, and the string and int builtins took
thirteen more. 2 of them mention an unsolved type variable (`$0`, `$1`, …) — a
generic parameter the binder gave up on. Name resolution is done apart from the
five bare `Symbol X not found`, which are all `File` and `Path` from
`using system.io`.

**`No operator` is at zero, and it was the largest class on the board for
most of this effort. `Symbol X not found for type T` went from 16 to 3 with
the builtins.** What is left is 19 conversions, 4 derivation reports that
should stay, 4 argument counts, 3 member lookups and 2 overloads.

`Cannot convert` is now more than half the board and 13 of the 19 are generic
inference, so for the first time since this count started there is a single
group worth attacking: §2.1.

| Count | File                        |
| ----: | --------------------------- |
|     6 | `Binder.pn`                 |
|     6 | `ExprBinder.pn`             |
|     5 | `Lowered.pn`                |
|     4 | `Emitter.pn`                |
|     4 | `Transpiler.pn`             |
|     3 | `compilation.pn`            |
|     2 | four files at two           |
|     1 | three files at one          |

`Ast.pn` and `Binder.pn` were the two largest at 62 and 61 while derivation was
blocked, being mostly declarations. `TypeInference.pn` was the largest at 24
and is now at zero; `Parser.pn` was 26 until the wildcard imports came out, and
is at one; `TextLineParser.pn` and `ArgsParser.pn` went to zero with string
indexing; `DiagnosticBag.pn` and `VM.pn` were 8 and 7 until the string
conversions; `Trim.pn`, `TextLocation.pn`, `SourceFile.pn` and
`IndentedStringBuilder.pn` went to zero with the builtins. What remains is
shaped by what the binder cannot do rather than by what it cannot prove.

### Nothing is ever written to disk

`Compilation.emit(output)` takes an output path
([`compilation.scala:97`](pncs/src/main/scala/compilation.scala:97)) and
ignores it. `Emitter.emit()` builds a chunk and metadata tables in memory and
returns them ([`Emitter.scala:156`](pncs/src/main/scala/Emitter.scala:156));
the only code in the repo that writes files is the transpiler.

So `pncs output.pnb source.pn` — the invocation in the README, in
`ArgsParser.printUsage()`, and in the docs — produces no `output.pnb`. There is
no `.pnb` reader either, and no CLI path to `Compilation.exec()`, even though
the VM works and `VmTests` drives it in-process for 76 tests.

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

Ordered by what the counts say, not by what is interesting. This list covers
the 33 that are not derivation; the 4 derivation reports are §1.3a.

1. **Generic inference** (see §2). 13 diagnostics: 2 reference an unsolved type
   variable, 11 a parameter that defaulted to `any` — `Dictionary<any, any>`
   and `Chain.Empty<any>` are most of them, and both come from a call with
   explicit type arguments that the binder does not apply. Two annotations
   trace to this: `bindEnumCases` has to state `val typ: Type` because the
   first argument to fix a type variable wins
   ([`Binder.scala:2486`](pncs/src/main/scala/Binder.scala:2486)), and
   `bindFunction` has to state `val genTypeParams: List[GenericTypeParameter]`
   because a `match` does not widen its branches.
2. **`File` and `Path`** — the five bare `Symbol X not found`, all from
   `using system.io`, which exists only in the Scala runtime.
3. **Member lookup on a case type** — the 3 remaining
   `Symbol X not found for type T`: `reverse` on `List.Cons<string>` and
   `List.Cons<GenericParameterSyntax>`, `concat` on `Chain.Singleton<$0>`.
   A case knows its enum; lookup never asks it.
4. **Overloads** — the 2 `Duplicate definition`, `inferCall` and `printNew`.
   Either rename them or add overload resolution. See §1.1.

Six items are closed. **The string and int builtins are done, 13 of them.**
`substring` (5), `compareTo` on `string` (2) and on `int` (2), `nonEmpty` (2),
`endsWith` and `toString` on `bool`. Three needed no builtin at all: `nonEmpty`
is `!= ""`, following `.isEmpty` becoming `== ""` in the pass before, and
`b.toString()` is `string(b)`, which already emitted `ConvStr`. The other four
are members on the builtin symbol emitted as their own opcode, the way
`string(value)` is `ConvStr` rather than a call — `Substr`, `EndsWith`, and
`Cmp` for both `compareTo`s, dispatching on the values it pops the way `Ceq`
and `Clt` already do. `substring` is the two-argument form only, because
Panther has no overloading, so the two callers that wanted Java's one-argument
overload now pass the length.

Doing it turned up a runtime hole the diagnostics could not see: **`"abc".length`
bound cleanly and crashed**, because a string's length is not a field there is a
token for, so it emitted `Ldfld` against a `Value.String`. Nothing had ever
executed it. `Ldlen` answers it now, the way it already did for an array and
the way `Ldelem` was extended to index one. Both callers of `substring` read
`.length` in the same expression, so the builtins would have been unrunnable
without it.

**`string + T` is at zero, written out by hand.**
Scala coerces the right operand of `+` through `toString`; Panther declares
only `string + string` and expects `string(x)`. Rather than paper over that in
the operator table or the transpiler, all 47 call sites now say what they
mean — which is what the docs have always taught, and identical in Scala,
where `string(x)` *is* `x.toString`. Seven of the 47 were a different bug
wearing the same message: `x.toString` without parens is a method reference in
Panther, typed `() -> string`, so the complaint was that you cannot
concatenate a function.

Doing it by hand surfaced seven more errors the `+` failures had been masking,
all of them the same kind of Scala-ism: `s.length()` called a field, `.isEmpty`
on a string does not exist, and a `match` whose branches do not widen. Those
are fixed too. Two `Cannot convert from any to int` are left, and they are
item 1.

**String indexing yields `char`.** `str(0)` used to
fall through to `apply`, which is the `string(…)` conversion, so it typed as
`string` and `str(0) == '-'` compared a string to a char. The index and the
conversion are the same shape — a call whose callee types as `string` — and
are told apart by whether the callee is the type itself or a value of it. That
took 18, and closed the runtime side with it: `char` literals emitted a
`panic("unimplemented")` before this, so no program containing one could run.
A char is an int at runtime, which the VM had already settled for `ConvChar`
and `getDefaultValueForType`, so a literal is its code point, `Ldelem` reads
one out of a `Value.String`, and `char - char`, `char + int` and `char - int`
answer in int. `ArgsParser.parseInt` lost a `return`, which Panther does not
have and which only became reachable once the comparisons around it bound.

**`if`/`else` now forms a union.** With no expected
type it infers both branches and unions them, exactly as `match` unions its
cases; `checkIf` already checked both branches against an expected type where
one exists. That took 21 `Cannot convert`, all of them a case against a
sibling case of one enum — `Option.None` against `Option.Some<T>`,
`MetadataFlags.None` against `MetadataFlags.Static`. It also retired two
`// annotated:` comments that existed only to work around it. A union of an
enum's cases already converted back to the enum, so nothing downstream needed
to change.

**`Type X not defined`** is at zero: the transpiler
rewrites `String`, `Boolean` and `Unit` in a type position, the enum cases used
unqualified as types went with the wildcard imports that made them reachable,
and `TypeInference` no longer reaches for `scala.collection.mutable.HashMap`.
**`Invalid namespace`** is at zero with it — all 18 were three per `HashMap`
occurrence, one for each prefix of `scala.collection.mutable`. The binder's
known gap in `using` resolution
([`Binder.scala:773`](pncs/src/main/scala/Binder.scala:773)) turns out not to
have been reporting any of them.

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

### 1.3a Burn down the derivation reports

**Done.** 4 left of a peak of 222, and they are `ConversionClassifier` and
`AstPrinter` — one parameter each, and both *should* stay unprovable. The fix
is at the call site, not in derivation. See the fourth pass of ADR 0004 for why
a reference-identity fallback would be wrong.

Everything else closed: conditional givens
([ADR 0006](docs/architecture/adr/0006-conditional-givens.md)), `[derive(…)]`
on a generic class and a generic enum, the transpiler emitting the attribute
for `enum`, a goal naming an enum case widening to the enum, a hand-written
`Eq[Array[T]]`, a parameter whose type is a composite over a type variable, and
the two fields whose type was an error because the transpiler left `Boolean`
and `String` alone.

Track the number after every change:

```bash
sbt pnc/compile
```

**37 → 0.** Nothing else in this section matters until that number moves.

Only the first 20 diagnostics are printed. To see them all, transpile first —
`pnc/compile` does this implicitly, and the count depends on it — then run the
compiler directly with a higher limit:

```bash
sbt pncs/transpile && sbt --error "pncs/run --diagnostics-limit 5000 out.pnb $(find pnc/src -name '*.pn' | tr '\n' ' ')"
```

### 1.3b Make derivation cheaper on the stack

A derived `Eq` over a recursive type is the most stack-hungry thing the VM
runs. Measured, for a list of `n` elements:

| | slots |
| --- | --- |
| derived `Eq` | `20 + 16n` |
| plain recursive function | `15 + 12n` |

A frame is `arguments + receiver + 3 + locals`, the 3 being the return address
and the caller's `localp` and `argsp`. The derived `==` measures
`args=3, locals=5` — 11 slots — and each level costs 16, the difference being
what the caller still has live on the stack across the call.

Four of the sixteen are evidence: the `$ev$self` argument every given's member
takes ([ADR 0006](docs/architecture/adr/0006-conditional-givens.md), decision
C) and the record being loaded twice at a `Calli`, once as that argument and
once to read the token out of. Three things are worth trying, in order of how
much they look worth:

1. **Five locals for a body that is a chain of `&&` over field comparisons.**
   Lowering spills more temporaries than the shape needs; this is the largest
   single line item and it is not specific to derivation.
2. **The record is loaded twice per evidence call.** `emitEvidenceCall` could
   `Dup` instead, trading a slot of stack for an opcode.
3. **`Eq` on a list is not naturally recursive** — `derivedEqChain` recurses
   because the *type* is recursive, but the comparison is a fold. A derived
   member that loops rather than recurses would be flat in `n`. This is the
   real fix and the largest change.

`Eq[Array[T]]` in the prelude is the third of these done by hand: it loops, so
its stack cost does not grow with the array's length. Whatever a derived member
would have to generate for `List` is what that body already spells out.

None of this blocks anything: the default stack is 8192 slots, which is about
680 plain frames or a derived `Eq` over a 500-element list. It was 50, which
is two or three frames of anything, and it went unnoticed because nothing
recursive had ever run on the VM.

Fixing `while` was a prerequisite for that body and is worth recording on its
own: the lowerer placed the loop's start label above every statement the
enclosing block had already lowered, not just above the condition, so a `var`
declared before a loop was re-initialised on each pass and a counter never
advanced. No test covered `while` at all. `Array.length` was the other one —
it was read as a field and landed on element 0, so a fresh array always looked
empty. Both are in
[ADR 0006](docs/architecture/adr/0006-conditional-givens.md).

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
other shape, `string + T` for non-string `T`, is decided too, and the answer
was neither of the two options that were on the table. Rather than give the
operator an overload against `any` or have the transpiler insert the call, the
47 sites were rewritten to say `string(x)` — which is what the docs teach,
what `BinderTests` pins, and what Scala was doing implicitly all along, since
`string(x)` is `x.toString`. The implicit form stays a diagnostic, so nothing
can drift back.

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
- **Test coverage is stage-shaped, not feature-shaped.** 451 tests, but
  `MetadataTests` has 2 and there is no end-to-end test that takes source all
  the way to output. §3.4 is the fix.

---

## Suggested order

Sequenced so each step makes the next one measurable.

**First — stop flying blind.** Done. The generated tree matches the
transpiler (§1.1), the exit code is trustworthy (§1.2), and failures come back
as diagnostics rather than exceptions (§4.2). The 37 counts every error the
front end finds — none are discarded.

**Second — generics.** The measurement overtook this plan twice, caught up
with it, and the builtins have now cleared the other pole out of the way.
**13 of the non-derivation 33 are generics**, and with `Symbol X not found for
type T` down to 3 they are the only group left worth calling a group: 2 mention
a type variable, 11 a parameter that defaulted to `any`. §2.1 and §2.2 are the
next thing, and §1.3 item 1 is the same work seen from the diagnostics side.

Derivation was the exception, and it is finished: 222 down to 4, over
[ADR 0006](docs/architecture/adr/0006-conditional-givens.md) and the passes
listed in §1.3a.

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
| Self-hosting diagnostics          |          37 |      0 | `sbt pnc/compile` (now fails, as it should)  |
| — of those, derivation            |           4 |      0 | §1.3a                                       |
| Doc blocks that fail              | **0 / 201** |      0 | `sbt "doccheck/run docs/src/content/docs"`  |
| Doc blocks skipped as unsupported |           2 |      0 | as above                                    |
| Samples that run in CI            |           0 |      6 | not yet built                               |
