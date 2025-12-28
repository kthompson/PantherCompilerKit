---
title: Functions
description: Learn how to define and use functions in Panther
---

Functions are the building blocks of Panther programs. They allow you to organize code into reusable units.

## Function Declaration

Define a function using the `fun` keyword:

```panther
fun greet(name: String): String {
    return "Hello, " + name + "!"
}

val message = greet("World")
println(message)  // Prints: Hello, World!
```

## Function Syntax

The general syntax for functions:

```panther
fun functionName(param1: Type1, param2: Type2): ReturnType {
    // function body
    return value
}
```

## Parameters

Functions can take multiple parameters:

```panther
fun add(a: Int, b: Int): Int {
    return a + b
}

fun introduce(firstName: String, lastName: String, age: Int): String {
    return firstName + " " + lastName + " is " + age + " years old"
}
```

## Return Types

### Explicit Return

Use the `return` keyword to return a value:

```panther
fun multiply(x: Int, y: Int): Int {
    return x * y
}
```

### Unit Functions

Functions that don't return a value have a `Unit` return type (similar to `void` in other languages):

```panther
fun printMessage(message: String): Unit {
    println(message)
}

// Unit can be omitted
fun printNumber(n: Int) {
    println(n)
}
```

## Expression Bodies

For simple functions, you can use expression syntax:

```panther
fun square(x: Int): Int = x * x

fun isEven(n: Int): Bool = n % 2 == 0

fun max(a: Int, b: Int): Int = if (a > b) a else b
```

## Local Functions

Functions can be nested inside other functions:

```panther
fun outer(x: Int): Int {
    fun inner(y: Int): Int {
        return y * 2
    }
    
    return inner(x) + 1
}
```

## Recursion

Panther supports recursive functions:

```panther
fun factorial(n: Int): Int {
    if (n <= 1) {
        return 1
    }
    return n * factorial(n - 1)
}

fun fibonacci(n: Int): Int {
    if (n <= 1) {
        return n
    }
    return fibonacci(n - 1) + fibonacci(n - 2)
}
```

## Higher-Order Functions

Functions can take other functions as parameters:

```panther
fun applyTwice(f: (Int) => Int, x: Int): Int {
    return f(f(x))
}

fun increment(x: Int): Int = x + 1

val result = applyTwice(increment, 5)  // Returns 7
```

## Anonymous Functions (Lambdas)

Define functions inline without a name:

```panther
val double = (x: Int) => x * 2

val sum = (a: Int, b: Int) => a + b

// Use with higher-order functions
val result = applyTwice((x: Int) => x * 2, 3)  // Returns 12
```

## Method Calls

Functions can be called on objects using dot notation:

```panther
val text = "hello"
val upper = text.toUpperCase()
val length = text.length()
```

## Best Practices

1. **Keep functions small and focused** - Each function should do one thing well
2. **Use descriptive names** - Function names should clearly indicate their purpose
3. **Minimize side effects** - Prefer pure functions when possible
4. **Document complex functions** - Use comments to explain non-obvious behavior

## Next Steps

- [Flow Control](/guides/flow-control) - Learn about conditional logic and loops
- [Data Types](/guides/data-types) - Explore Panther's type system
