---
title: Variables
description: Working with values and mutability in Panther
---

# Variables

Variables allow you to store and manipulate data in your programs.

## Immutable Values with val

Use `val` to declare immutable values:

```panther
val message = "Hello"
val count = 42
val price = 9999  // price in cents
```

A `val` is assigned once and cannot be reassigned:

```panther
val x = 10
println(x)
```

Reassigning one is a compile error:

<!-- panther-check: expect-error -->
```panther
val total = 10
total = 20  // error: reassignment to val total
```

Use `var` (below) for anything you intend to reassign.

## Mutable Variables with var

Use `var` for variables that need to change:

```panther
var counter = 0
counter = counter + 1  // OK
counter = 5            // OK
```

## Type Annotations

Panther has type inference, but you can explicitly specify types:

```panther
val name: string = "Panther"
val age: int = 42
val letter: char = 'P'
val isActive: bool = true
```

## Type Inference

The compiler automatically infers types:

```panther
val text = "hello"      // Inferred as string
val number = 42         // Inferred as int
val letter = 'h'        // Inferred as char
val flag = true         // Inferred as bool
```

## Identifiers

A name starts with an ASCII letter or `_`, followed by any mix of letters,
digits, and `_`. Names are case-sensitive, so `count` and `Count` are different
names:

```panther
val itemCount = 5
val _hidden = 1
val Count = 2
```

A name cannot start with a digit:

<!-- panther-check: expect-error -->
```panther
val 2nd = "second"  // error: Unexpected token NumberToken, expected IdentifierToken
```

## Scope

Variables are scoped to the block they're declared in:

<!-- panther-check: expect-error -->
```panther
{
    val outer = "outside"
    
    {
        val inner = "inside"
        println(outer)  // OK - can access outer scope
    }
    
    println(inner)  // error: Symbol inner not found
}
```
