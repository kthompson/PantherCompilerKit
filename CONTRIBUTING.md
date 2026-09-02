# Contributing to the Panther Compiler Kit

Thanks for your interest in the Panther Compiler Kit. This document covers the
development loop, the invariants a change has to respect, and what CI expects
before a pull request can merge.

Start with the [README](README.md) for setup, the build commands, and the
repository layout. The one thing to carry over from it: **`pncs/` (Scala) is the
canonical compiler, and `pnc/` (Panther) is generated from it.**

## Development loop

```bash
sbt test/test        # establish a baseline before you change anything
```

Then, while working:

```bash
sbt pncs/compile     # compile the Scala compiler
sbt test/test        # run the suites
```

Iterate on a single suite with:

```bash
sbt "test/testOnly BinderTests"
```

## Invariants

### `pncs/` is canonical; `pnc/` is generated

Never hand-edit files under `pnc/src/`. They are overwritten by
`sbt pncs/transpile`. Fix the Scala source, re-transpile, and commit the result.
A change that touches `pncs/`, `runtime/`, `metadata/`, or `text/` without a
matching `pnc/src/` update will fail CI.

### Working within the Panther subset

The transpiler handles a subset of Scala 3. Code that compiles under
`sbt pncs/compile` can still be untranspilable, and the failure shows up later,
in the transpile or self-hosting stages. Follow the conventions already in the
codebase:

- Use the `panther._` types (`string`, `int`, `char`, `unit`, `any`) rather than
  their Scala equivalents, and the `system.io._` wrappers for file and path IO.
- Prefer `case class`, `enum`, plain `def`, `match`, and explicit recursion.
  Avoid implicits/`given`, higher-kinded types, for-comprehensions over custom
  types, and the parts of the Scala collections library that the runtime does
  not mirror.
- Prefer the project's own `List`, `Array`, and `StringBuilder` helpers over
  `scala.collection`.
- When in doubt, find an existing file in `pncs/src/main/scala/` that does
  something similar and follow its shape.

If you add a language construct to the compiler, make sure the transpiler can
emit it before relying on it inside the compiler's own sources.

## Code style

- Formatting is enforced by **scalafmt 3.9.1** with the `scala3` dialect
  ([`.scalafmt.conf`](.scalafmt.conf)). Run `sbt scalafmtAll`; CI runs
  `sbt scalafmtCheckAll`.
- [`.editorconfig`](.editorconfig) sets the rest: UTF-8, LF endings, final
  newline, trimmed trailing whitespace, 2-space indent, 120-column Scala lines.
- The build compiles with `-Xfatal-warnings`, so warnings break the build. Fix
  them rather than suppressing them.
- One concept per file, named after the concept — the flat layout of
  `pncs/src/main/scala/` is intentional and keeps the `.pn` twins aligned.

## Tests

Tests live in `test/src/test/scala/` and use **ScalaTest** with `AnyFunSpec`
and `Matchers`:

```scala
class LexerTests extends AnyFunSpec with Matchers {
  describe("Lexer") {
    it("should handle single token") {
      val tokens = mkTokens("1")
      tokens.length shouldEqual 2
    }
  }
}
```

Shared construction helpers (`mkTokens`, `mkSyntaxTree`, `mkCompilation`,
`mkBinaryExpr`, and friends) are in
[`TestHelpers.scala`](test/src/test/scala/TestHelpers.scala) — use them instead
of rebuilding a lexer or compilation by hand.

Suites are organized by pipeline stage: `LexerTests`, `ParserTests`,
`BinderTests`, `TypeTests`, `MetadataTests`, `VmTests`, `ArgsParserTests`. Add
new cases to the suite for the stage you changed, and add a test for every bug
you fix.

## Documentation code blocks

Every ` ```panther ` block under `docs/` is compiled by
[`doccheck`](tools/doccheck/README.md):

```bash
sbt "doccheck/run docs/src/content/docs"
```

Every block currently compiles, and `tools/doccheck/baseline.txt` — the list of
blocks known not to compile — is empty. Keep it that way: a snippet you add or
change has to pass.

The check fails in two directions — a snippet that used to compile breaking,
*and* a baselined snippet starting to compile — so if you ever do need to
baseline something, regenerate it in the same commit:

```bash
sbt "doccheck/run --update-baseline docs/src/content/docs"
```

A snippet that should not be compiled as written gets a directive on the line
above its fence — `parse-only`, `expect-error`, or `skip reason="..."`. Prefer
fixing the snippet; see [ROADMAP.md](ROADMAP.md#4-documentation-that-is-checked)
for the plan to empty the baseline.

## Commits

Commit messages follow a Conventional-Commits style, with an optional scope
naming the pipeline stage:

```
feat(binder): improve bidirectional type checking
fix: use Type.Error for unknown identifier patterns
refactor(tests): convert tests to use AnyFunSpec
docs: update docs to use astro
```

Common types in this repo: `feat`, `fix`, `refactor`, `docs`, `test`, `chore`.
Keep the subject in the imperative mood and under ~72 characters.

## Pull requests

Run these before you push — they are exactly what CI runs:

```bash
sbt scalafmtAll
```

```bash
sbt pncs/transpile
```

```bash
sbt pncs/compile && sbt test/test && sbt scalafmtCheckAll
```

```bash
sbt "doccheck/run docs/src/content/docs"
```

Then **commit the regenerated `.pn` files** along with your Scala changes.

The [`ci`](.github/workflows/ci.yml) workflow runs four jobs on every pull
request, all of which must pass:

1. `sbt pncs/compile`, then `sbt test/test`
2. `sbt pncs/transpile`, then `git diff --exit-code` — this fails if the
   transpiled output in `pnc/src/` is out of sync with the Scala sources
3. `doccheck` against its baseline
4. `sbt scalafmtCheckAll`

For the pull request itself:

- Keep it focused on one change; separate mechanical reformatting from behavior
  changes so the diff stays reviewable.
- Describe what changed and why, and note any new or changed diagnostics.
- Include the regenerated `pnc/src/` output in the same PR.
- Say explicitly if you changed a compiler invariant or the shape of a pipeline
  stage.

### Optional: the system recap block

For non-trivial changes, the repo has a `visual-recap` skill
([`.claude/skills/visual-recap/SKILL.md`](.claude/skills/visual-recap/SKILL.md))
that generates a visual summary of which pipeline primitives a change touches
and how risky it is, rendered inline in the PR description. It's informational
and non-blocking — it supplements review, it doesn't replace reading the diff.

If your change **adds, removes, or materially reshapes** a primitive (a new
compilation stage, a split or renamed module, changed code roots), update
[`docs/architecture/primitives.yaml`](docs/architecture/primitives.yaml) in the
same PR and validate it:

```bash
node .claude/skills/visual-recap/scripts/check-primitives-map.mjs
```

Ordinary feature work inside an existing stage does **not** need a taxonomy
edit — that's what commit messages and PR descriptions are for.
