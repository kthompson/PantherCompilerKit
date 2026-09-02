---
title: Parameters
description: Working with function parameters in Panther
---

Parameters allow you to pass data into functions.

## Basic Parameters

Define parameters with a name and type:

```panther
def greet(name: string): unit = {
    println("Hello, " + name)
}

greet("Alice")
```

## Multiple Parameters

Functions can have multiple parameters:

```panther
def add(a: int, b: int): int = {
    a + b
}

def introduce(firstName: string, lastName: string, age: int): string = {
    firstName + " " + lastName + " is " + string(age) + " years old"
}

val result = introduce("John", "Doe", 30)
```

## Parameter Order

Arguments must be passed in the order parameters are defined:

```panther
def divide(numerator: int, denominator: int): int = {
    numerator / denominator
}

val result = divide(10, 2)  // 5
// NOT: divide(2, 10)  // Would give 0
```

## Type Annotations Required

All parameters must have explicit type annotations:

```panther
// Correct
def square(x: int): int = {
    x * x
}
```

<!-- panther-check: expect-error -->
```panther
// Error: parameter type required
def square(x) = {  // Error!
    x * x
}
```

## Parameter Reassignment

Parameters are ordinary local bindings, so they can be reassigned inside the function body:

```panther
def increment(x: int): int = {
    x = x + 1  // Allowed - x behaves like a local variable
    x
}
```

Many style guides still prefer introducing a new value instead of mutating a parameter, since it keeps the original argument visible for the rest of the function:

```panther
def incrementPure(x: int): int = {
    val result = x + 1
    result
}
```

## Parameter Scope

Parameters are scoped to the function body:

```panther
def example(x: int): int = {
    val y = x * 2
    y
}

// x and y are not accessible here
```

## Variable Number of Parameters

While Panther doesn't have built-in varargs, you can use arrays:

```panther
def sum(numbers: Array[int]): int = {
    var total = 0
    for (i <- 0 to numbers.length - 1) {
        total = total + numbers(i)
    }
    total
}

val numbers = new Array[int](5)
numbers(0) = 1
numbers(1) = 2
numbers(2) = 3
numbers(3) = 4
numbers(4) = 5

val result = sum(numbers)  // 15
```

## Function Parameters

Panther does not yet have function types or lambdas, so a function cannot be declared to take
another function as a parameter (see [Higher-Order Functions](higher-order-functions) for what
is and isn't possible today).

## Common Patterns

### Validation

```panther
enum DivideResult {
    case Ok(value: int)
    case Err(message: string)
}

def divide(numerator: int, denominator: int): DivideResult = {
    if (denominator == 0) {
        DivideResult.Err("Division by zero")
    } else {
        DivideResult.Ok(numerator / denominator)
    }
}
```

### Transformation

```panther
def double(x: int): int = x * 2

def negate(x: int): int = -x
```

### Predicate Functions

```panther
def isPositive(n: int): bool = n > 0

def isEven(n: int): bool = n % 2 == 0

def isEmpty(text: string): bool = text.length == 0
```
