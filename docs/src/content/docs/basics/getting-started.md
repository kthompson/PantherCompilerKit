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
println("Version: " + version)
```

## Compiling Your Code

Use the Panther compiler to compile your code:

```bash
pncs output.pnb source.pn
```

This compiles `source.pn` to bytecode in `output.pnb`.

## Running Your Program

After compilation, run your program with the PVM (Panther Virtual Machine):

```bash
pvm output.pnb
```

## Next Steps

- Learn about [variables](variables) to store and manipulate data
- Explore [operators](operators) for calculations and comparisons
