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
def add(a: int, b: int): int {
    return a + b
}

def introduce(firstName: string, lastName: string, age: int): string {
    return firstName + " " + lastName + " is " + age + " years old"
}

val result = introduce("John", "Doe", 30)
```

## Parameter Order

Arguments must be passed in the order parameters are defined:

```panther
def divide(numerator: int, denominator: int): int {
    return numerator / denominator
}

val result = divide(10, 2)  // 5
// NOT: divide(2, 10)  // Would give 0
```

## Type Annotations Required

All parameters must have explicit type annotations:

```panther
// Correct
def square(x: int): int {
    return x * x
}

// Error: parameter type required
def square(x) {  // Error!
    return x * x
}
```

## Immutable Parameters

Parameters are immutable by default:

```panther
def increment(x: int): int = {
    x = x + 1  // Error: Cannot reassign parameter
    x
}

// Instead, create a new variable
def increment(x: int): int = {
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
def sum(numbers: Array<int>): int = {
    var total = 0
    for (num in numbers) {
        total = total + num
    }
    total
}

val result = sum([1, 2, 3, 4, 5])  // 15
```

## Function Parameters

Functions can take other functions as parameters (see [Higher-Order Functions](higher-order-functions)):

```panther
def applyOperation(x: int, y: int, op: (int, int) => int): int = {
    op(x, y)
}

def add(a: int, b: int): int = a + b

val result = applyOperation(5, 3, add)  // 8
```

## Common Patterns

### Validation

```panther
def divide(numerator: int, denominator: int): Result<int, string> = {
    if (denominator == 0) {
        Err("Division by zero")
    } else {
        Ok(numerator / denominator)
    }
}
```

### Transformation

```panther
def toUpperCase(text: string): string {
    // Convert text to uppercase
}

def double(x: int): int = x * 2
```

### Predicate Functions

```panther
def isPositive(n: int): bool = n > 0

def isEven(n: int): bool = n % 2 == 0

def isEmpty(text: string): bool = text.length() == 0
```
