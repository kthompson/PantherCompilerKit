---
title: Defining Functions
description: Basic function syntax and structure in Panther
---

Functions encapsulate reusable blocks of code.

## Basic Syntax

Define a function using the `def` keyword:

```panther
def functionName(param1: int, param2: string): bool = {
    // function body
    param1 > 0  // Last expression is returned
}
```

## Simple Functions

A function with no parameters:

```panther
def sayHello(): unit = {
    println("Hello!")
}

sayHello()  // Call the function
```

## Functions with Parameters

Pass data to functions via parameters:

```panther
def greet(name: string): unit = {
    println("Hello, " + name)
}

greet("Alice")
greet("Bob")
```

## Functions with Return Values

The last expression in a function is automatically returned:

```panther
def add(a: int, b: int): int = {
    a + b
}

val sum = add(5, 3)  // sum is 8
```

## Expression Body Syntax

For simple functions, use expression syntax:

```panther
def square(x: int): int = x * x

def isEven(n: int): bool = n % 2 == 0

def max(a: int, b: int): int = if (a > b) a else b
```

This is equivalent to:

```panther
def square(x: int): int = {
    x * x
}
```

## Unit Functions

Functions that don't return a meaningful value have type `unit`:

```panther
def printMessage(msg: string): unit = {
    println(msg)
}

// Return type can be omitted, but = is still required
def printNumber(n: int) = {
    println(n)
}
```

## Local Functions (not supported)

Panther does not currently support defining a function inside another function's body — `def`
is only valid at the top level or as a class/object member. To split logic the way a nested
helper would in other languages, define a second top-level function instead:

```panther
def double(y: int): int = {
    y * 2
}

def outer(x: int): int = {
    double(x) + 1
}

val result = outer(5)  // 11
```

Because Panther has no function types or lambdas (see [Higher-Order Functions](higher-order-functions)),
functions also cannot close over a variable and be handed back as a value. Every function's
behavior is fixed by its top-level or class-member definition.

## Recursive Functions

Functions can call themselves:

```panther
def factorial(n: int): int = {
    if (n <= 1) {
        1
    } else {
        n * factorial(n - 1)
    }
}

val result = factorial(5)  // 120
```

## Function Naming

Follow these conventions:

- Use camelCase: `calculateTotal`, `getUserName`
- Start with a verb: `getUser`, `calculateArea`, `isValid`
- Be descriptive: `validateEmailAddress` over `validate`

```panther
class User(id: int)

// Good
def calculateArea(width: int, height: int): int = width * height

def isValidEmail(email: string): bool = email.length > 0

def getUserById(id: int): User = new User(id)

// Avoid
def calc(w: int, h: int): int = w * h

def check(s: string): bool = s.length > 0

def get(x: int): User = new User(x)
```

## Pure Functions

Pure functions have no side effects and always return the same output for the same input:

```panther
// Pure function
def add(a: int, b: int): int = {
    a + b
}

// Impure function (has side effect)
def printAndAdd(a: int, b: int): int = {
    println("Adding numbers")  // Side effect
    a + b
}
```
