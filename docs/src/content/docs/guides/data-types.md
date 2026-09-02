---
title: Data Types
description: Explore Panther's type system and built-in data types
---

Panther is a statically-typed language with a rich type system. Understanding types is essential for writing correct and efficient Panther code.

## Primitive Types

### Integer Type

Panther has a single integer type, `int`, a 32-bit signed integer. There is no separate `Byte`, `Short`, or `Long` type, and no numeric literal suffixes:

```panther
val count: int = 2147483647
```

### No Floating-Point Type

Panther does not currently have a floating-point type (no `float` or `double`). Numeric computation is done with `int`.

### Boolean Type

```panther
val isTrue: bool = true
val isFalse: bool = false
```

### Character Type

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

### String Operations

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

## Array Type

Arrays are fixed-size collections created with `new Array[T](size)`. There are no array literals, so elements are assigned individually. Indexing uses parentheses, not brackets:

```panther
// Array initialization
val numbers = new Array[int](5)
numbers(0) = 1
numbers(1) = 2
numbers(2) = 3
numbers(3) = 4
numbers(4) = 5

// Access elements
val first = numbers(0)
val second = numbers(1)

// Modify elements
numbers(0) = 10

// Array length
val size = numbers.length
```

## List Type

Panther's standard library has a `List[T]` type, but it is not a language built-in and has to be defined like any other type. It is a classic recursive "cons list": either `Nil` (empty) or `Cons(head, tail)`:

```panther
enum IntList {
  case Nil
  case Cons(head: int, tail: IntList)
}

val numbers = IntList.Cons(1, IntList.Cons(2, IntList.Cons(3, IntList.Nil)))

val first = numbers match {
  case IntList.Cons(head, _) => head
  case IntList.Nil => 0
}

println(string(first))
```

## Option Type

An `Option` type represents values that may or may not exist. Panther has no built-in `Option`, but it is easy to define as a generic enum with two cases:

```panther
enum Option[in T] {
  case None
  case Some(value: T)
}

val some: Option[int] = Option.Some(42)
val none: Option[int] = Option.None

// Pattern matching
val result = some match {
  case Option.Some(value) => value
  case Option.None => 0
}

println(string(result))
```

## Result Type

A `Result` type represents operations that can succeed or fail. Like `Option`, it is defined with an enum rather than being built into the language:

```panther
enum DivideResult {
  case Ok(value: int)
  case Err(error: string)
}

def divide(a: int, b: int): DivideResult = {
  if (b == 0) {
    DivideResult.Err("Division by zero")
  } else {
    DivideResult.Ok(a / b)
  }
}

val result = divide(10, 2)

val message = result match {
  case DivideResult.Ok(value) => "Result: " + string(value)
  case DivideResult.Err(error) => "Error: " + error
}

println(message)
```

## Grouping Values

There is no built-in tuple syntax (no `(int, string)` type and no `(1, "a")` literal), but a small class does the same job:

```panther
class Pair[A, B](first: A, second: B)

val pair = new Pair[int, string](42, "answer")
val first = pair.first    // 42
val second = pair.second  // "answer"
```

## Custom Types

### Classes

Define custom data types using classes. Constructor parameters are automatically accessible as fields:

```panther
class Person(name: string, age: int) {
  def greet(): string = "Hello, my name is " + name
}

val person = new Person("Alice", 30)
println(person.greet())
```

### Enums

Enumerations define a type with a fixed set of values. Each case goes on its own line, and matches must use the qualified name:

```panther
enum Color {
  case Red
  case Green
  case Blue
}

val color: Color = Color.Red

val name = color match {
  case Color.Red => "Red"
  case Color.Green => "Green"
  case Color.Blue => "Blue"
}

println(name)
```

### Enums With Data (Discriminated Unions)

There is no separate `union` keyword — an enum case can carry its own fields, which is how Panther expresses discriminated unions:

```panther
enum Shape {
  case Circle(radius: int)
  case Rectangle(width: int, height: int)
  case Triangle(base: int, height: int)
}

def area(shape: Shape): int = shape match {
  case Shape.Circle(r) => r * r * 3
  case Shape.Rectangle(w, h) => w * h
  case Shape.Triangle(b, h) => b * h / 2
}

val circle = Shape.Circle(5)
val circleArea = area(circle)
println(string(circleArea))
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

## Generic Types

Create reusable types with type parameters, written in square brackets:

```panther
class Box[T](value: T) {
  def get(): T = value
}

val intBox = new Box[int](42)
val stringBox = new Box[string]("hello")

println(string(intBox.get()))
println(stringBox.get())
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

## Best Practices

1. **Use type inference** - Let the compiler infer types when obvious
2. **Prefer immutability** - Use `val` over `var` when possible
3. **Use Option and Result-shaped enums** - Model absence or failure instead of relying on `null`
4. **Leverage pattern matching** - Destructure enum and class values safely
5. **Keep types simple** - Deeply generic types can run into inference limits (see note below)

> **Note:** The examples above that involve generics keep to a single type parameter used directly, because the current compiler's type inference for generics defined and used within one small snippet is limited — passing a generic value through a second generic function, or using two type parameters at once, can fail to infer correctly even when the shapes match. Panther's real standard library (`Option`, `List`, `Result`-like types) is generic; these gaps mostly show up when writing small, self-contained generic code rather than in the full compiler build.

## Next Steps

- [Basics](/guides/basics) - Review fundamental concepts
- [Functions](/guides/functions) - Learn about generic functions
- [Flow Control](/guides/flow-control) - Master pattern matching with types
