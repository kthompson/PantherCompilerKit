# Panther Compiler Kit

A multi-stage compiler for **Panther**, a language that compiles to PVM
(Panther Virtual Machine) bytecode.

The kit contains two compilers for the same language:

- **`pncs`** — the compiler written in Scala 3. This is the canonical source.
- **`pnc`** — the compiler written in Panther itself. It is **generated** from
  the Scala sources by `sbt pncs/transpile` and checked into the repo.
  Stage 3 verifies that it compiles itself to the same bytecode image as `pncs`.

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

This writes `output.pnb`. To compile and execute a program in one step:

```sh
sbt "pncs/run --run samples/hello.pn"
```

Compiled images can be loaded through `ImageRunner`; a standalone `pvm`
command is still on the [roadmap](ROADMAP.md#32-add-a-runner--mostly-done).

### Standard library

`Option`, `Result`, `Either`, `List`, `Dictionary`, and `Tuple2` are automatically
available to user programs. Both compilers carry the source bundle, so the
default works from any working directory. Emitted images include the library.
Use `--no-stdlib` for isolated compilations, or `--stdlib <directory>` to replace
the bundle. See [the stdlib guide](stdlib/README.md) for the required files.

### Regenerating the Panther sources

Any change to the Scala sources must be transpiled and the result committed —
CI checks that `pnc/src/` and `stdlib/src/` are in sync.

```sh
sbt pncs/transpile
```

### Building and verifying the self-hosted compiler

```sh
sbt pnc/compile stage3/run
```

`pnc/compile` regenerates the Panther sources and compiles them with `pncs`
to `pnc/target/pnc.pnb` (Stage 2). `stage3/run` runs that image over the same
sources, writes `pnc/target/pnc-stage3.pnb`, and requires byte-for-byte equality.

The fixed point was verified on 2026-10-10 using a 32M-slot managed heap;
see [the measurements](ROADMAP.md#15-run-the-stages). The CI compiler job
requires this fixed point, with a 15-minute job timeout. The verifier runs in
a separate JVM capped at 4 GiB; managed heap slots do not measure total
process memory.

### Scripts

`scripts/` holds cross-platform PowerShell wrappers around the same sbt tasks:
`tests.ps1`, `lint.ps1`, and the bootstrap stages `stage0.ps1` (transpile),
`stage1.ps1` (legacy Scala-source bootstrap), `stage2.ps1` (`pnc/compile`),
and `stage3.ps1` (build Stage 2 and verify the self-hosting fixed point).
Use Stage 2 followed by Stage 3 for the verified self-hosting workflow.

## Repository layout

| Path        | Contents                                                                    |
| ----------- | --------------------------------------------------------------------------- |
| `pncs/`     | The Panther compiler in Scala — lexer, parser, binder, lowering, emitter, VM |
| `pnc/`      | **Generated** Panther sources (`.pn`) produced by `sbt pncs/transpile`       |
| `runtime/`  | Panther standard library (and the `panther._` Scala shim)                    |
| `stdlib/`   | Generated user library sources embedded in both compilers                   |
| `metadata/` | Metadata reading/writing library                                            |
| `text/`     | Text processing library                                                     |
| `test/`     | ScalaTest suites plus shared helpers                                        |
| `docs/`     | Astro/Starlight documentation site                                          |
| `tools/`    | Development tooling — doc code-block checks and the Stage 3 verifier      |
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

The latest check reports 201 blocks: 199 pass and 2 are explicitly skipped
for unsupported higher-order functions. `tools/doccheck/baseline.txt` is empty;
CI rejects new failures.

## Roadmap

[ROADMAP.md](ROADMAP.md) tracks completed milestones and remaining work in
self-hosting, generics, and runnable sample programs, with measured results.

## Contributing

See [CONTRIBUTING.md](CONTRIBUTING.md) for the development loop, the invariants
a change has to respect, code style, testing conventions, and the commit and
pull request process.
