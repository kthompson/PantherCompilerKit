---
title: Data Types
description: Explore Panther's type system and built-in data types
---

Panther is a statically-typed language with a rich type system. Understanding types is essential for writing correct and efficient Panther code.

## Primitive Types

### Integer Types

Panther supports various integer types:

```panther
val byte: Byte = 127           // 8-bit signed integer (-128 to 127)
val short: Short = 32767       // 16-bit signed integer
val int: Int = 2147483647      // 32-bit signed integer (default)
val long: Long = 9223372036854775807L  // 64-bit signed integer
```

### Floating-Point Types

```panther
val float: Float = 3.14f       // 32-bit floating point
val double: Double = 3.14159   // 64-bit floating point (default)
```

### Boolean Type

```panther
val isTrue: Bool = true
val isFalse: Bool = false
```

### Character Type

```panther
val letter: Char = 'A'
val digit: Char = '5'
val symbol: Char = '@'
```

## String Type

Strings represent sequences of characters:

```panther
val message: String = "Hello, Panther!"
val empty: String = ""
val multiLine: String = """
    This is a
    multi-line
    string
"""
```

### String Operations

```panther
val greeting = "Hello"
val name = "World"

// Concatenation
val message = greeting + ", " + name + "!"

// Length
val len = message.length()

// Substring
val sub = message.substring(0, 5)

// Character at index
val char = message.charAt(0)

// String interpolation
val interpolated = "Hello, ${name}!"
```

## Array Type

Arrays are fixed-size collections of elements:

```panther
// Array initialization
val numbers: Array<Int> = [1, 2, 3, 4, 5]
val names: Array<String> = ["Alice", "Bob", "Charlie"]

// Access elements
val first = numbers[0]
val second = numbers[1]

// Modify elements
numbers[0] = 10

// Array length
val size = numbers.length
```

## List Type

Lists are dynamic collections:

```panther
val fruits = List.of("apple", "banana", "cherry")

// Add elements
val moreFruits = fruits.add("date")

// Get element
val first = fruits.get(0)

// Size
val count = fruits.size()

// Check if empty
val isEmpty = fruits.isEmpty()
```

## Option Type

The `Option` type represents values that may or may not exist:

```panther
val some: Option<Int> = Some(42)
val none: Option<Int> = None

// Pattern matching
val result = match (some) {
    Some(value) => value * 2
    None => 0
}

// Methods
val hasValue = some.isSome()
val hasNoValue = none.isNone()
val unwrapped = some.getOrElse(0)
```

## Result Type

The `Result` type represents operations that can succeed or fail:

```panther
fun divide(a: Int, b: Int): Result<Int, String> {
    if (b == 0) {
        return Err("Division by zero")
    }
    return Ok(a / b)
}

val result = divide(10, 2)

match (result) {
    Ok(value) => println("Result: " + value)
    Err(error) => println("Error: " + error)
}
```

## Tuple Types

Tuples group multiple values together:

```panther
// Pair (2-tuple)
val pair: (Int, String) = (42, "answer")
val first = pair._1   // 42
val second = pair._2  // "answer"

// Triple (3-tuple)
val triple: (Int, String, Bool) = (1, "hello", true)
val a = triple._1
val b = triple._2
val c = triple._3
```

## Custom Types

### Classes

Define custom data types using classes:

```panther
class Person {
    val name: String
    val age: Int
    
    fun new(name: String, age: Int): Person {
        this.name = name
        this.age = age
    }
    
    fun greet(): String {
        return "Hello, my name is " + name
    }
}

val person = Person.new("Alice", 30)
println(person.greet())
```

### Records

Records are immutable data structures:

```panther
record Point(x: Int, y: Int)

val origin = Point(0, 0)
val point = Point(10, 20)

// Access fields
val xCoord = point.x
val yCoord = point.y
```

### Enums

Enumerations define a type with a fixed set of values:

```panther
enum Color {
    Red,
    Green,
    Blue
}

val color: Color = Color.Red

match (color) {
    Color.Red => println("Red")
    Color.Green => println("Green")
    Color.Blue => println("Blue")
}
```

### Discriminated Unions

Define types with multiple variants:

```panther
union Shape {
    Circle(radius: Float),
    Rectangle(width: Float, height: Float),
    Triangle(base: Float, height: Float)
}

fun area(shape: Shape): Float {
    return match (shape) {
        Circle(r) => 3.14159 * r * r
        Rectangle(w, h) => w * h
        Triangle(b, h) => 0.5 * b * h
    }
}

val circle = Shape.Circle(5.0)
val area = area(circle)
```

## Type Inference

Panther can infer types automatically:

```panther
// Type is inferred as Int
val number = 42

// Type is inferred as String
val text = "hello"

// Type is inferred as List<Int>
val numbers = [1, 2, 3]

// Type is inferred from function return type
fun getAge(): Int = 25
val age = getAge()  // age is Int
```

## Generic Types

Create reusable types with type parameters:

```panther
class Box<T> {
    val value: T
    
    fun new(value: T): Box<T> {
        this.value = value
    }
    
    fun get(): T {
        return value
    }
}

val intBox = Box.new(42)
val stringBox = Box.new("hello")
```

## Type Aliases

Create alternative names for types:

```panther
type UserId = Int
type UserName = String
type Coordinate = (Float, Float)

val id: UserId = 12345
val name: UserName = "Alice"
val position: Coordinate = (10.5, 20.3)
```

## Nullable Types

Explicitly handle nullable values:

```panther
// Non-nullable (default)
val name: String = "Alice"

// Nullable
val optionalName: String? = null

// Safe navigation
val length = optionalName?.length()

// Elvis operator
val len = optionalName?.length() ?: 0

// Null check
if (optionalName != null) {
    // Smart cast: optionalName is String here
    println(optionalName.length())
}
```

## Type Casting

Convert between types:

```panther
// Implicit conversion (when safe)
val int: Int = 42
val long: Long = int  // Int automatically widens to Long

// Explicit conversion
val double: Double = int.toDouble()
val string: String = int.toString()

// Type checking
if (value is String) {
    // value is automatically cast to String
    println(value.length())
}

// As operator
val text = value as String
```

## Best Practices

1. **Use type inference** - Let the compiler infer types when obvious
2. **Prefer immutability** - Use `val` over `var` when possible
3. **Use Option and Result** - Avoid null when representing absence or failure
4. **Leverage pattern matching** - Destructure complex types safely
5. **Keep types simple** - Avoid deeply nested generic types
6. **Use type aliases** - Make domain types more expressive

## Next Steps

- [Basics](/guides/basics) - Review fundamental concepts
- [Functions](/guides/functions) - Learn about generic functions
- [Flow Control](/guides/flow-control) - Master pattern matching with types
