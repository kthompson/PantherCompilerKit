# doccheck

Checks that every ` ```panther ` code block in the documentation is actually
valid Panther.

The docs are the language's public contract, so every snippet in them has to
survive the compiler. `doccheck` walks the markdown, extracts each Panther
block, and pushes it through the real `pncs` front end — the same lexer,
parser, and binder the compiler uses. Binding includes the same bundled standard
library as user compilations, so snippets can use `Option`, `List`, `Dictionary`,
and `Result` directly.

## Running it

```bash
sbt "doccheck/run docs/language docs/architecture"
```

Paths may be files or directories; directories are walked for `.md` and
`.mdx`.

Every checked block compiles. A run reports what it found:

```
panther doc check  (stage: Bind)
  33 files, 197 panther blocks

  197 blocks: 184 ok, 0 failing, 6 skipped, 7 ignored
```

`0 failing` is the invariant CI enforces; the totals move as docs are added.
The skips are on `functions/higher-order-functions.md`, which documents
lambdas and function-typed parameters — neither is implemented.

[`baseline.txt`](baseline.txt) records blocks that are known not to compile and
is **picked up automatically**, so a run reports only what has changed. It is
**empty**; keep it that way.

Useful flags:

| Flag | Effect |
| --- | --- |
| `--stage <parse\|bind>` | How far to take each block. `parse` catches syntax only; `bind` (the default) also resolves symbols and types. |
| `--no-baseline` | Ignore the baseline and report every failure, baselined or not. |
| `--show-known` | Detail the baselined failures instead of just counting them. |
| `--baseline <file>` | Use a different baseline file. |
| `--update-baseline` | Rewrite the baseline from this run. |
| `--update` | Rewrite `// error:` comments and `output` fences from this run. |
| `--show-passing` | List passing blocks too, not just failures. |
| `--max-problems <n>` | Diagnostics shown per block (default 5; `0` for all). |

Exit code is `0` when everything the run cares about passed, `1` on a failure
or a malformed directive, `2` on bad arguments.

## Per-block directives

Not every snippet should be compiled as-is. Put an HTML comment on the line
before the fence — markdown renderers drop it, so it does not show up on the
docs site:

````markdown
<!-- panther-check: parse-only -->
```panther
println(somethingDefinedElsewhere)
```
````

| Directive | Meaning |
| --- | --- |
| `check` | Parse and bind; any diagnostic fails. The default, so it rarely needs writing out. |
| `parse-only` | Syntax must be valid, but symbols are not resolved. For fragments that reference things the block does not define. |
| `expect-error` | An intentional error. The block must produce a diagnostic, and each diagnostic must appear as a trailing `// error: ...` comment on its line (see below). |
| `ignore reason="..."` | A known, unintended failure, usually a feature that is not built yet. Compiled, but its diagnostics do not fail the run. Once the block compiles cleanly the run fails, so the directive gets removed. A reason is required. |
| `skip reason="..."` | Not compiled at all, e.g. for an elided `...` body. A reason is required. |

### Intentional errors

An `expect-error` block shows the reader what the compiler says:

````markdown
<!-- panther-check: expect-error -->
```panther
val total = 10
total = 20  // error: reassignment to val total
```
````

The comments are checked: if the compiler's diagnostics change, the run fails.
Write or refresh them with:

```bash
sbt "doccheck/run --update docs/language docs/architecture"
```

### Output

An ```` ```output ```` fence directly after a `panther` block makes doccheck
compile **and run** the block. It must print exactly the fence's contents
(trailing newlines aside), and must not crash or exit non-zero:

````markdown
```panther
for (i <- 1 to 3) { println(string(i * i)) }
```

```output
1
4
9
```
````

Leave the fence empty and `--update` fills it in; it also rewrites a fence
whose output has changed. Only normally checked blocks can have one.

### Hidden context

A `panther-context` comment holds code the page needs but does not show. Every
`panther` block after it on the same page compiles against it, and several
context comments on one page add up:

````markdown
<!-- panther-context
class User(name: string, age: int)
-->

```panther
val user = new User("Ada", 36)
println(user.name)
```
````

The context is prepended to each block's source, so it can hold anything a
block could, including top-level `def`s and `val`s. Line numbers in reports and
`// error:` comments still point at the right lines. An error in the context
fails every block that uses it and is reported against the comment's lines.
`parse-only` blocks do not see the context.

A typo in a directive is reported as a failure rather than silently ignored,
so a misspelled mode cannot quietly switch checking off.

## The baseline

The baseline is a ratchet. A run fails when a block outside the baseline
produces a diagnostic — and also when a baselined block compiles cleanly, so
the list cannot silently go stale. In that case, regenerate it:

```bash
sbt "doccheck/run --update-baseline docs/language docs/architecture"
```

The baseline path is resolved relative to the working directory, so run this
from the repository root.

The baseline should only ever get shorter: CI runs against it, so an entry
there is a snippet nobody is checking.

## Limitations

- Blocks are compiled one at a time. A `panther-context` comment can supply
  shared declarations, but a block cannot continue the statements of the
  previous one, so multi-block walkthroughs still need `parse-only` on the
  fragments.
- Only blocks with an `output` fence are run. The rest stop after binding, so
  output claimed in a `// Prints:` comment is still unchecked.
