---
title: Variables
description: Working with values and mutability in Panther
---

Variables allow you to store and manipulate data in your programs.

## Immutable Values with val

Use `val` to declare immutable values:

```panther
val message = "Hello"
val count = 42
val price = 99.99
```

Once assigned, `val` variables cannot be changed:

```panther
val x = 10
x = 20  // Error: Cannot reassign immutable value
```

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
val price: float = 99.99
val isActive: bool = true
```

## Type Inference

The compiler automatically infers types:

```panther
val text = "hello"      // Inferred as string
val number = 42         // Inferred as int
val decimal = 3.14      // Inferred as float
val flag = true         // Inferred as bool
```

## Naming Conventions

- Use camelCase for variable names: `userName`, `itemCount`
- Use descriptive names that indicate purpose
- Avoid single-letter names except for loop counters

```panther
// Good
val userName = "Alice"
val itemCount = 5

// Avoid
val u = "Alice"
val x = 5
```

## Scope

Variables are scoped to the block they're declared in:

```panther
{
    val outer = "outside"
    
    {
        val inner = "inside"
        println(outer)  // OK - can access outer scope
    }
    
    println(inner)  // Error - inner is not in scope
}
```
