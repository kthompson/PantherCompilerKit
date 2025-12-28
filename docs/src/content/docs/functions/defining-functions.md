---
title: Defining Functions
description: Basic function syntax and structure in Panther
---

Functions encapsulate reusable blocks of code.

## Basic Syntax

Define a function using the `def` keyword:

```panther
def functionName(param1: Type1, param2: Type2): ReturnType = {
    // function body
    value  // Last expression is returned
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

## Local Functions (future)

Define functions inside other functions:

```panther
def outer(x: int): int = {
    def inner(y: int): int = {
        y * 2
    }
    
    inner(x) + 1
}

val result = outer(5)  // 11
```

Local functions can access variables from the enclosing scope:

```panther
def makeAdder(n: int): (int) => int = {
    def add(x: int): int = {
        x + n  // Accesses n from outer scope
    }
    add
}

val add5 = makeAdder(5)
val result = add5(10)  // 15
```

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
// Good
def calculateArea(width: float, height: float): float

def isValidEmail(email: string): bool

def getUserById(id: int): User

// Avoid
def calc(w: float, h: float): float

def check(s: string): bool

def get(x: int): User
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
