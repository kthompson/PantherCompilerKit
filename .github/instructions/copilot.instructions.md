---
applyTo: "**"
---

# Panther Compiler Kit

**Always reference these instructions first and fallback to search or bash commands only when you encounter unexpected information that does not match the info here.**

The Panther Compiler Kit is a multi-stage compiler system written in Scala that compiles a custom language called Panther to PVM (Panther Virtual Machine) bytecode. The project includes both a Scala-based compiler (pncs) and a self-hosted compiler written in Panther itself (pnc).

## Working Effectively

### Prerequisites and Setup

Install required dependencies in this exact order:

```bash
# Verify Java 17+ is available (should already be installed)
java -version

# Install sbt manually (package managers may fail)
curl -fL https://github.com/sbt/sbt/releases/download/v1.10.7/sbt-1.10.7.tgz | tar -xzf - -C /tmp
export PATH=/tmp/sbt/bin:$PATH

# Verify sbt works (takes 8-15 seconds on first run)
sbt version
```

### Core Build Commands

**CRITICAL: NEVER CANCEL BUILDS OR TESTS. Set timeouts of 5+ minutes for safety.**

#### Working Commands (Validated)

```bash
# Compile the Scala-based Panther compiler
sbt pncs/compile
# Takes: ~8 seconds. NEVER CANCEL. Timeout: 120+ seconds.

# Run comprehensive test suite
sbt test/test
# Takes: ~16 seconds, all tests should pass. NEVER CANCEL. Timeout: 300+ seconds.

# Transpile Scala code to Panther (.pn files)
sbt pncs/transpile
# Takes: ~8 seconds. NEVER CANCEL. Timeout: 120+ seconds.

# Format all Scala code
sbt scalafmtAll
# Takes: ~16 seconds. NEVER CANCEL. Timeout: 300+ seconds.

# Check code formatting (used by CI)
sbt scalafmtCheckAll
# Takes: ~9 seconds. NEVER CANCEL. Timeout: 120+ seconds.

# Show compiler help
sbt "pncs/run --help"
# Takes: ~10 seconds. Shows full usage with options and examples

# Check every ```panther block in the docs compiles
sbt "doccheck/run docs/src/content/docs"
# Takes: ~30 seconds. Baseline is empty, so any failure is a real regression.
```

#### Commands That Do Not Produce A Working Compiler

```bash
# DO NOT USE for validation:
sbt compile        # Fails during the pnc/compile step
sbt pnc/compile    # Reports ~1058 diagnostics against the generated .pn
                   # sources. It exits ZERO regardless - read the diagnostic
                   # count, not the exit status.
sbt pncs/bootstrap # Fails during the transpilation step
```

Self-hosting is not finished; [ROADMAP.md](../../ROADMAP.md) tracks what is
left.

### Cross-Platform Scripts

Use PowerShell scripts for consistent cross-platform execution:

```bash
# Stage 0: Transpile Scala to Panther
pwsh scripts/stage0.ps1  # Equivalent to: sbt pncs/transpile

# Stage 1: Bootstrap (does not succeed yet)
pwsh scripts/stage1.ps1  # Equivalent to: sbt pncs/bootstrap

# Stage 2: Compile Panther to bytecode (reports diagnostics, see above)
pwsh scripts/stage2.ps1  # Equivalent to: sbt pnc/compile

# Run tests
pwsh scripts/tests.ps1   # Equivalent to: sbt test/test

# Format code
pwsh scripts/lint.ps1    # Equivalent to: sbt scalafmtAll
```

## Validation Scenarios

### Always Test After Changes

1. **Build validation**: `sbt pncs/compile` (must succeed)
2. **Test validation**: `sbt test/test` (all tests must pass)
3. **Format validation**: `sbt scalafmtCheckAll` (must pass for CI)
4. **Transpile validation**: `sbt pncs/transpile` (should complete without errors)
5. **Doc validation**: `sbt "doccheck/run docs/src/content/docs"` (0 failing
   blocks; required by CI)

### Manual Testing Scenarios

```bash
# Test basic compiler functionality
sbt "pncs/run --help"
# Shows full usage with options:
#   Usage: pncs [options] <output> <sources...>
#   Options include: --help, --transpile, --debug, --trace, --print-symbols,
#   --print-bound-assembly, --print-lowered-assembly, --stack-size, --heap-size,
#   --recovery-attempts, --diagnostics-limit
#   Examples: pncs output.pnb source1.scala source2.scala
#             pncs --transpile output/ source1.scala source2.scala

# Test compiler with minimal input (create test file first)
echo "val x = 42" > /tmp/test.pn
sbt "pncs/run /tmp/output.pnb /tmp/test.pn"
# Should compile without errors and show symbol tree

# Verify transpiled code was generated (after pncs/transpile)
ls -la pnc/src/*.pn | wc -l
# Should show ~92 .pn files after successful transpilation
```

## Project Structure

### Key Components

- **pncs/** - Panther Compiler in Scala (main compiler, compiles to PVM bytecode)
- **pnc/** - Panther Compiler in Panther (self-hosted compiler using .pn files)
- **runtime/** - Panther Standard Library
- **metadata/** - Metadata reading/writing library
- **text/** - Text processing library
- **test/** - Test project (ScalaTest suites, one per pipeline stage)
- **docs/** - Astro/Starlight documentation site (Node and pnpm needed only to
  run the site locally)
- **tools/** - Development tooling, currently `doccheck`, the doc code-block
  checker

### Build Configuration

- **build.sbt** - Main build configuration for all subprojects
- **project/** - SBT build configuration (build.properties, plugins.sbt)
- **.scalafmt.conf** - Code formatting rules (Scala 3.9.1, scala3 dialect)

### Important Files

- **pncs/src/main/scala/Program.scala** - Main entry point for Scala compiler
- **pnc/src/Program.pn** - Main entry point for Panther compiler (transpiled)
- **.github/workflows/ci.yml** - CI pipeline (runs compile, test, transpile, lint)

## Development Workflow

### Making Changes

1. **Always run tests first**: `sbt test/test` to establish baseline
2. **Make focused changes** to Scala files in appropriate subproject
3. **Compile frequently**: `sbt pncs/compile`
4. **Test changes**: `sbt test/test`
5. **Complete your work**: See "Completing Your Work" section below

### Completing Your Work

When you're done making changes, always run these commands in order:

1. **Format all code**: `sbt scalafmtAll`

   - Cleans up formatting across all Scala source files
   - Required for CI to pass

2. **Transpile to Panther**: `sbt pncs/transpile`

   - Converts any updated Scala sources to Panther (.pn files)
   - Keeps the self-hosted compiler in sync

3. **Final validation**: `sbt scalafmtCheckAll`
   - Verifies formatting is correct for CI
   - Must pass before committing changes

### Key Patterns

- Main compiler logic is in `pncs/src/main/scala/`
- Tests are in `test/src/test/scala/` using ScalaTest (`AnyFunSpec` +
  `Matchers`)
- Test helpers are in `test/src/test/scala/TestHelpers.scala`
- Transpiled Panther code appears in `pnc/src/` as `.pn` files

## Common Issues

### Build Failures

- If sbt is not found: Install manually using curl command above
- If compile fails: Only use `sbt pncs/compile`, not `sbt compile`
- If tests fail: Check that you haven't broken existing functionality
- If format check fails: Run `sbt scalafmtAll` to fix formatting

### Timing Expectations

- **sbt version**: ~6 seconds (first run may take 8-15s for dependency downloads)
- **pncs/compile**: ~8 seconds per command
- **test/test**: ~8-16 seconds (all must pass)
- **pncs/transpile**: ~8 seconds (writes ~92 .pn files)
- **doccheck/run**: ~30 seconds
- **scalafmtAll**: ~8-16 seconds
- **scalafmtCheckAll**: ~7-9 seconds
- **NEVER CANCEL** these operations - they need time to complete
- **PowerShell scripts**: Add ~3-5 seconds overhead to equivalent sbt commands

### CI Pipeline Requirements

The GitHub CI pipeline runs four jobs and ALL must pass:

1. `sbt pncs/compile`, then `sbt test/test`
2. `sbt pncs/transpile` (then checks for git diff)
3. `sbt "doccheck/run docs/src/content/docs"`
4. `sbt scalafmtCheckAll`

Always run these locally before pushing changes.
