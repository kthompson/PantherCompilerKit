---
title: Functions
description: Learn how to define and use functions in Panther
---

Functions are the building blocks of Panther programs. They allow you to organize code into reusable units.

## Function Declaration

Define a function using the `def` keyword:

```panther
def greet(name: string): string = "Hello, " + name + "!"

val message = greet("World")
println(message)  // Prints: Hello, World!
```

## Function Syntax

The general syntax for functions:

```panther
def functionName(param1: int, param2: string): bool = {
  // function body — the last expression is the result
  param1 > 0
}
```

## Parameters

Functions can take multiple parameters:

```panther
def add(a: int, b: int): int = a + b

def introduce(firstName: string, lastName: string, age: int): string =
  firstName + " " + lastName + " is " + string(age) + " years old"
```

Note the `string(age)` conversion. Panther has no automatic conversion from
`int` to `string`, so concatenating a number requires it explicitly.

## Return Values

Panther has no `return` keyword. A function's value is its body — for a block
body, the last expression:

```panther
def multiply(x: int, y: int): int = x * y

def describe(n: int): string = {
  val doubled = n * 2
  "twice " + string(n) + " is " + string(doubled)
}
```

### Unit Functions

Functions that do not produce a value have the `unit` return type, the
equivalent of `void` in other languages:

```panther
def printMessage(message: string): unit = println(message)

def printNumber(n: int): unit = println(string(n))
```

## Expression Bodies

Simple functions read well as a single expression:

```panther
def square(x: int): int = x * x

def isEven(n: int): bool = n % 2 == 0

def max(a: int, b: int): int = if (a > b) a else b
```

`if` is an expression in Panther, so it can be a function body directly.

## Recursion

Panther supports recursive functions:

```panther
def factorial(n: int): int = if (n <= 1) 1 else n * factorial(n - 1)

def fibonacci(n: int): int =
  if (n <= 1) n else fibonacci(n - 1) + fibonacci(n - 2)

println(string(factorial(5)))   // Prints: 120
println(string(fibonacci(10)))  // Prints: 55
```

Recursion is the main tool for iteration over recursive data, since Panther's
`for` loop only counts over a numeric range.

## Not yet supported

Three things that functions in other languages have, and Panther does not yet:

**Nested functions.** A `def` cannot appear inside another function's body.
Declare helpers at the top level instead:

```panther
def double(y: int): int = y * 2

def outer(x: int): int = double(x) + 1
```

**Function-typed parameters.** There is no way to declare a parameter that
takes a function, so higher-order functions cannot be written yet.

**Anonymous functions.** There are no lambdas — `(x: int) => x * 2` does not
parse.

All three are tracked in the repository's `ROADMAP.md`.

## Calling Methods

Methods are called with dot notation. Note that `length` on a `string` is a
field, not a method, so it takes no parentheses:

```panther
val text = "hello"
println(string(text.length))  // Prints: 5
```

## Best Practices

1. **Keep functions small and focused** - Each function should do one thing well
2. **Use descriptive names** - Function names should clearly indicate their purpose
3. **Minimize side effects** - Prefer pure functions when possible
4. **Document complex functions** - Use comments to explain non-obvious behavior

## Next Steps

- [Flow Control](/guides/flow-control) - Learn about conditional logic and loops
- [Data Types](/guides/data-types) - Explore Panther's type system
