---
title: Primitive Types
description: Integers, booleans, characters, and strings
---

# Primitive Types

Panther's built-in scalar types are `int`, `bool`, `char`, and `string`.

## Integer Type

Panther has a single integer type, `int`, a 32-bit signed integer. There is no separate `Byte`, `Short`, or `Long` type, and no numeric literal suffixes:

```panther
val count: int = 2147483647
```

## No Floating-Point Type

Panther does not currently have a floating-point type (no `float` or `double`). Numeric computation is done with `int`.

## Boolean Type

```panther
val isTrue: bool = true
val isFalse: bool = false
```

## Character Type

```panther
val letter: char = 'A'
val digit: char = '5'
val symbol: char = '@'
```

## String Type

Strings represent sequences of characters:

```panther
val message: string = "Hello, Panther!"
val empty: string = ""
```

## String Operations

Panther's `string` type supports concatenation and a `length` field. There is no string interpolation, so build messages with `+` and convert non-string values with `string(...)`:

```panther
val greeting = "Hello"
val name = "World"

// Concatenation
val message = greeting + ", " + name + "!"

// Length (a field, not a method call)
val len = message.length

println(message)
println("length: " + string(len))
```

## Type Inference

Panther can infer types automatically:

```panther
// Type is inferred as int
val number = 42

// Type is inferred as string
val text = "hello"

// Type is inferred from function return type
def getAge(): int = 25
val age = getAge()  // age is int
```

## Type Checking and Casting

Panther has `is` for runtime type checks and `as` for casts:

```panther
val value: any = "hello"

if (value is string) {
    println("it is a string")
}

val text = value as string
println(text)
```

Convert between primitive types with the `string(...)` and `int(...)` conversion functions rather than `.toString()`/`.toDouble()` style methods:

```panther
val n: int = 42
val asText: string = string(n)   // Explicit conversion to string
val backToInt: int = int(asText) // Explicit conversion back to int
```
