---
title: Return Values
description: Returning values from functions in Panther
---

Functions automatically return the value of their last expression.

## Basic Return

The last expression is automatically returned:

```panther
def add(a: int, b: int): int = {
    a + b
}

val sum = add(5, 3)  // sum is 8
```

## Return Type Annotation

The return type must be explicitly declared:

```panther
def multiply(x: int, y: int): int = {
    x * y
}

def greet(name: string): string = {
    "Hello, " + name
}
```

## Conditional Returns

Use if-else expressions to return different values:

```panther
def divide(a: int, b: int): int = {
    if (b == 0) {
        0  // Return 0 on error condition
    } else {
        a / b
    }
}
```

## Multiple Conditions

Chain if-else expressions:

```panther
def absoluteValue(x: int): int = {
    if (x < 0) {
        -x
    } else {
        x
    }
}

def getGrade(score: int): string = {
    if (score >= 90) {
        "A"
    } else if (score >= 80) {
        "B"
    } else if (score >= 70) {
        "C"
    } else {
        "F"
    }
}
```

## Expression Bodies

For single-expression functions, the `return` is implicit:

```panther
def square(x: int): int = x * x

def isEven(n: int): bool = n % 2 == 0

def max(a: int, b: int): int = if (a > b) a else b
```

This is exactly equivalent to:

```panther
def square(x: int): int = {
    x * x
}
```

## unit Return Type

Functions that don't return a meaningful value use `unit`:

```panther
def printMessage(msg: string): unit = {
    println(msg)
    // println returns unit
}

// Return type can be omitted, but = is still required
def logError(error: string) = {
    println("ERROR: " + error)
}
```

## Returning Complex Types

### Tuples

Return multiple values using tuples:

```panther
def divideWithRemainder(a: int, b: int): (int, int) = {
    val quotient = a / b
    val remainder = a % b
    (quotient, remainder)
}

val result = divideWithRemainder(10, 3)
val quotient = result._1  // 3
val remainder = result._2  // 1
```

### Custom Types

Return custom data structures:

```panther
record Point(x: float, y: float)

def createPoint(x: float, y: float): Point = {
    Point(x, y)
}

val origin = createPoint(0.0, 0.0)
```

### Collections

Return arrays or lists:

```panther
def getFirstThree(): Array<int> = {
    [1, 2, 3]
}

def getNames(): List<string> = {
    List.of("Alice", "Bob", "Charlie")
}
```

## Option Type

Use `Option` for values that might not exist:

```panther
def findUser(id: int): Option<User> = {
    if (userExists(id)) {
        Some(getUser(id))
    } else {
        None
    }
}

val user = findUser(123)
match (user) {
    Some(u) => println("Found: " + u.name)
    None => println("User not found")
}
```

## Result Type

Use `Result` for operations that can fail:

```panther
def parseNumber(text: string): Result<int, string> = {
    if (isValidNumber(text)) {
        Ok(toInt(text))
    } else {
        Err("Invalid number format")
    }
}

val result = parseNumber("42")
match (result) {
    Ok(num) => println("Number: " + num)
    Err(msg) => println("Error: " + msg)
}
```

## Multiple Expressions

When a function has multiple statements, the last expression is returned:

```panther
def calculateDiscount(price: float, percentage: float): float = {
    // Validation
    if (price <= 0) {
        0
    } else if (percentage < 0 || percentage > 100) {
        price
    } else {
        // Main logic - last expression is returned
        price * (1 - percentage / 100)
    }
}
```

## Common Patterns

### Validation with Early Return

```panther
def validateUser(user: User): Result<User, string> = {
    if (user.name.isEmpty()) {
        Err("Name is required")
    } else if (user.age < 0) {
        Err("Age must be positive")
    } else if (user.email.isEmpty()) {
        Err("Email is required")
    } else {
        Ok(user)
    }
}
```

### Transformation

```panther
def transformData(input: string): string = {
    val trimmed = input.trim()
    val upper = trimmed.toUpperCase()
    upper
}

// Or as expression
def transformData(input: string): string = 
    input.trim().toUpperCase()
```

### Computation with Multiple Steps

```panther
def calculateFinalPrice(basePrice: float, tax: float, discount: float): float = {
    val priceWithTax = basePrice * (1 + tax)
    val discountAmount = priceWithTax * discount
    val finalPrice = priceWithTax - discountAmount
    finalPrice
}
```
