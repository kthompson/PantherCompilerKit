---
title: Panther Basics
description: Learn the fundamental concepts of the Panther programming language
---

Panther is a statically-typed programming language that compiles to PVM (Panther Virtual Machine) bytecode. This guide covers the essential building blocks of the language.

## Hello World

The simplest Panther program:

```panther
println("Hello, World!")
```

## Variables

Panther uses `val` for immutable values and `var` for mutable variables:

```panther
// Immutable value
val message = "Hello"

// Mutable variable
var count = 0
count = count + 1
```

## Type Annotations

While Panther has type inference, you can explicitly specify types:

```panther
val name: String = "Panther"
val age: Int = 42
val price: Float = 99.99
val isActive: Bool = true
```

## Comments

Panther supports single-line and multi-line comments:

```panther
// This is a single-line comment

/* This is a
   multi-line comment */
```

## Basic Operations

### Arithmetic

```panther
val sum = 10 + 5        // 15
val difference = 10 - 5 // 5
val product = 10 * 5    // 50
val quotient = 10 / 5   // 2
val remainder = 10 % 3  // 1
```

### Comparison

```panther
val isEqual = 10 == 10      // true
val isNotEqual = 10 != 5    // true
val isGreater = 10 > 5      // true
val isLess = 5 < 10         // true
val isGreaterOrEqual = 10 >= 10  // true
val isLessOrEqual = 5 <= 10      // true
```

### Logical

```panther
val andResult = true && false  // false
val orResult = true || false   // true
val notResult = !true          // false
```

## String Operations

Panther provides built-in string manipulation:

```panther
val greeting = "Hello, " + "World!"  // String concatenation
val length = greeting.length()       // Get string length
```

## Output

Use `println` to print values with a newline:

```panther
println("Hello!")
println(42)
println(true)
```

## Next Steps

Now that you understand the basics, explore:
- [Functions](/guides/functions) - Learn how to define and use functions
- [Flow Control](/guides/flow-control) - Master conditional logic and loops
- [Data Types](/guides/data-types) - Deep dive into Panther's type system
