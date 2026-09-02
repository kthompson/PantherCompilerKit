# doccheck

Checks that every ` ```panther ` code block in the documentation is actually
valid Panther.

The docs are the language's public contract, and until now nothing verified
that the snippets in them would survive the compiler. `doccheck` walks the
markdown, extracts each Panther block, and pushes it through the real `pncs`
front end — the same lexer, parser, and binder the compiler uses.

## Running it

```bash
sbt "doccheck/run docs/src/content/docs"
```

Paths may be files or directories; directories are walked for `.md` and
`.mdx`.

All 198 checked blocks currently compile:

```
panther doc check  (stage: Bind)
  22 files, 200 panther blocks

  200 blocks: 198 ok, 0 failing, 2 skipped
```

The two skips are on `functions/higher-order-functions.md`, which documents
lambdas and function-typed parameters — neither is implemented.

[`baseline.txt`](baseline.txt) records blocks that are known not to compile and
is **picked up automatically**, so a run reports only what has changed. It
started at 157 entries and is now empty; keep it that way.

Useful flags:

| Flag | Effect |
| --- | --- |
| `--stage <parse\|bind>` | How far to take each block. `parse` catches syntax only; `bind` (the default) also resolves symbols and types. |
| `--no-baseline` | Ignore the baseline and report every failure. This is the ~800-line view of everything that is broken. |
| `--show-known` | Detail the baselined failures instead of just counting them. |
| `--baseline <file>` | Use a different baseline file. |
| `--update-baseline` | Rewrite the baseline from this run. |
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
| `expect-error` | The block *must* produce a diagnostic. For snippets that deliberately show a compile error. |
| `skip reason="..."` | Not checked at all. A reason is required. |

A typo in a directive is reported as a failure rather than silently ignored,
so a misspelled mode cannot quietly switch checking off.

## The baseline

The baseline turns a red wall into a ratchet. A run fails only when a block
that used to pass starts failing — and also when a baselined block starts
passing, so the list cannot silently go stale. In that case, regenerate it:

```bash
sbt "doccheck/run --update-baseline docs/src/content/docs"
```

The baseline path is resolved relative to the working directory, so run this
from the repository root.

The baseline should only ever get shorter. See
[`ROADMAP.md`](../../ROADMAP.md) for the plan to empty it.

## Limitations

- Blocks are compiled one at a time, in isolation. There is no way yet to say
  "this block continues the previous one", so multi-block walkthroughs need
  `parse-only` on the fragments.
- There is no prelude a block can opt into, so a snippet that wants a `User`
  type has to declare it.
- Checking stops after binding. Blocks are never lowered, emitted, or run, so
  a snippet can pass here and still not produce the output the prose claims.
  Executing blocks needs a `.pnb` writer and a runner first.
