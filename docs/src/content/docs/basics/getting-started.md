---
title: Getting Started
description: Write your first Panther program
---

## Hello World

The simplest Panther program:

```panther
println("Hello, World!")
```

## Program Structure

Panther programs are composed of declarations and statements. A basic program might look like:

```panther
// Variable declarations
val name = "Panther"
val version = 1

// Function calls
println("Welcome to " + name)
println("Version: " + string(version))
```

## Running Your Program

Compile and run in one step:

```bash
pncs --run source.pn
```

This compiles `source.pn` and executes it immediately, printing its output.
If your program calls `exit`, that code becomes the exit code of `pncs`.

## Compiling Your Code

To compile without running:

```bash
pncs output.pnb source.pn
```

This writes a bytecode image you can keep and run later. A separate `pvm`
command to load one from the shell does not exist yet, so `--run` is still the
way to execute a program from source.

## Next Steps

- Learn about [variables](variables) to store and manipulate data
- Explore [operators](operators) for calculations and comparisons
