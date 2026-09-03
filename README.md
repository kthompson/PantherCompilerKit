# Panther Compiler Kit

A multi-stage compiler for **Panther**, a language that compiles to PVM
(Panther Virtual Machine) bytecode.

The kit contains two compilers for the same language:

- **`pncs`** — the compiler written in Scala 3. This is the canonical source.
- **`pnc`** — the compiler written in Panther itself. It is **generated** from
  the Scala sources by `sbt pncs/transpile` and checked into the repo, on the
  way to a fully self-hosted compiler.

## Setup

1. **Install a JDK.** Java 17 or newer — [Temurin](https://adoptium.net/) is
   what CI uses.

2. **Install sbt.** From the
   [official download page](https://www.scala-sbt.org/download.html), or drop a
   release onto your `PATH`:

   ```sh
   curl -fL https://github.com/sbt/sbt/releases/download/v1.10.7/sbt-1.10.7.tgz | tar -xzf - -C /tmp && export PATH=/tmp/sbt/bin:$PATH
   ```

3. **Fetch dependencies.**

   ```sh
   sbt update
   ```

Node and pnpm are only needed if you are working on the documentation site.

## Getting started

### Building

```sh
sbt pncs/compile
```

### Running tests

```sh
sbt test/test
```

### Running the compiler

```sh
sbt "pncs/run --help"
```

```sh
sbt "pncs/run output.pnb source.pn"
```

Note that the output path is not honoured yet: the emitter builds the chunk and
metadata in memory and nothing is written to disk, so no `output.pnb` appears.
See [ROADMAP.md](ROADMAP.md#31-write-and-read-pnb).

### Regenerating the Panther sources

Any change to the Scala sources must be transpiled and the result committed —
CI checks that `pnc/src/` is in sync.

```sh
sbt pncs/transpile
```

### Commands that do not work yet

Self-hosting is not finished, so these do not produce a working compiler:

- `sbt compile` (use `sbt pncs/compile`)
- `sbt pnc/compile` — reports 996 diagnostics against the generated `.pn`
  sources and fails the build.
- `sbt pncs/bootstrap`

[ROADMAP.md](ROADMAP.md#1-self-hosting) tracks what is left.

### Scripts

`scripts/` holds cross-platform PowerShell wrappers around the same sbt tasks:
`tests.ps1`, `lint.ps1`, and the bootstrap stages `stage0.ps1` (transpile),
`stage1.ps1` (bootstrap), `stage2.ps1` (`pnc/compile`).

## Repository layout

| Path        | Contents                                                                    |
| ----------- | --------------------------------------------------------------------------- |
| `pncs/`     | The Panther compiler in Scala — lexer, parser, binder, lowering, emitter, VM |
| `pnc/`      | **Generated** Panther sources (`.pn`) produced by `sbt pncs/transpile`       |
| `runtime/`  | Panther standard library (and the `panther._` Scala shim)                    |
| `metadata/` | Metadata reading/writing library                                            |
| `text/`     | Text processing library                                                     |
| `test/`     | ScalaTest suites plus shared helpers                                        |
| `docs/`     | Astro/Starlight documentation site                                          |
| `tools/`    | Development tooling — currently `doccheck`, the doc code-block checker      |
| `scripts/`  | Cross-platform PowerShell wrappers around the sbt tasks                     |

Entry points: [`pncs/src/main/scala/Program.scala`](pncs/src/main/scala/Program.scala)
for the Scala compiler, and `pnc/src/Program.pn` for its transpiled twin.

## Documentation

The site under `docs/` is Astro + Starlight, with content in
`docs/src/content/docs/`:

```sh
pnpm --dir docs install && pnpm --dir docs dev
```

All documentation lives in `docs/`. (`docs_old/` is a stale mdbook tree pending
deletion — do not add to it or cite it.)

Every ` ```panther ` block in the docs is checked against the compiler by
[`doccheck`](tools/doccheck/README.md):

```sh
sbt "doccheck/run docs/src/content/docs"
```

Every block compiles, and `tools/doccheck/baseline.txt` — the list of
known-broken blocks — is empty, so CI fails if that stops being true.

## Roadmap

[ROADMAP.md](ROADMAP.md) covers the three things being worked toward —
self-hosting, generics, and runnable sample programs — with the current
measurements for each.

## Contributing

See [CONTRIBUTING.md](CONTRIBUTING.md) for the development loop, the invariants
a change has to respect, code style, testing conventions, and the commit and
pull request process.
