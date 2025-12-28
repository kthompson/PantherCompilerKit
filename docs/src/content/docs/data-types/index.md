---
title: Data Types
description: Explore Panther's type system and built-in data types
---

Panther is a statically-typed language with a rich type system. Understanding types is essential for writing correct and efficient Panther code.

## Topics

Explore Panther's type system:

- **[Primitive Types](primitive-types)** - Numbers, booleans, and characters
- **[Collections](collections)** - Arrays and lists
- **[Option and Result](option-and-result)** - Handling optional and fallible values
- **[Custom Types](custom-types)** - Classes, records, and enums

## Quick Example

```panther
val name: String = "Panther"
val age: Int = 42
val scores: Array<Int> = [95, 87, 92]
val user: Option<User> = Some(currentUser)
```
