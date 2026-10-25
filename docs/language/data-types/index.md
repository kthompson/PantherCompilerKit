---
title: Data Types
description: Explore Panther's type system and built-in data types
---

# Data Types

Panther is a statically-typed language with a rich type system. Understanding types is essential for writing correct and efficient Panther code.

## Topics

Explore Panther's type system:

- **[Primitive Types](primitive-types)** - Numbers, booleans, and characters
- **[Collections](collections)** - Arrays and lists
- **[Option and Result](option-and-result)** - Handling optional and fallible values
- **[Custom Types](custom-types)** - Classes, generic types, and enums
- **[Traits and Givens](traits-and-givens)** - Capabilities a type can provide

## Quick Example

```panther
val name: string = "Panther"
val age: int = 42
val scores: Array[int] = new Array[int](3)
scores(0) = 95
scores(1) = 87
scores(2) = 92
```
