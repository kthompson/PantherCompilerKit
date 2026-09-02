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

### Regenerating the Panther sources

Any change to the Scala sources must be transpiled and the result committed —
CI checks that `pnc/src/` is in sync.

```sh
sbt pncs/transpile
```

### Commands that are expected to fail

Self-hosting is not finished yet, so these do not currently succeed:

- `sbt compile` (fails in the `pnc/compile` step — use `sbt pncs/compile`)
- `sbt pnc/compile`
- `sbt pncs/bootstrap`

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
| `scripts/`  | Cross-platform PowerShell wrappers around the sbt tasks                     |

Entry points: [`pncs/src/main/scala/Program.scala`](pncs/src/main/scala/Program.scala)
for the Scala compiler, and `pnc/src/Program.pn` for its transpiled twin.

## Documentation

The site under `docs/` is Astro + Starlight, with content in
`docs/src/content/docs/`:

```sh
pnpm --dir docs install && pnpm --dir docs dev
```

`docs_old/` is the previous mdbook-based site, kept for reference. New
documentation goes in `docs/`.

## Contributing

See [CONTRIBUTING.md](CONTRIBUTING.md) for the development loop, the invariants
a change has to respect, code style, testing conventions, and the commit and
pull request process.
