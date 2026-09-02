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

### Multiple Values via a Class

Panther has no tuple type, so return multiple values by defining a small class:

```panther
class DivisionResult(quotient: int, remainder: int)

def divideWithRemainder(a: int, b: int): DivisionResult = {
    val quotient = a / b
    val remainder = a % b
    new DivisionResult(quotient, remainder)
}

val result = divideWithRemainder(10, 3)
val quotient = result.quotient  // 3
val remainder = result.remainder  // 1
```

### Custom Types

Return custom data structures. Note that Panther has no `float`/`double` type — use `int` for numeric fields:

```panther
class Point(x: int, y: int)

def createPoint(x: int, y: int): Point = {
    new Point(x, y)
}

val origin = createPoint(0, 0)
```

### Collections

Return arrays:

```panther
def getFirstThree(): Array[int] = {
    val result = new Array[int](3)
    result(0) = 1
    result(1) = 2
    result(2) = 3
    result
}

def getNames(): Array[string] = {
    val result = new Array[string](3)
    result(0) = "Alice"
    result(1) = "Bob"
    result(2) = "Charlie"
    result
}
```

## Modeling Optional Values

Panther doesn't ship a built-in `Option` type. Define an enum to represent a value that might not exist:

```panther
class User(name: string)

enum UserLookup {
    case Found(user: User)
    case NotFound
}

def userExists(id: int): bool = id > 0

def getUser(id: int): User = new User("Alice")

def findUser(id: int): UserLookup = {
    if (userExists(id)) {
        UserLookup.Found(getUser(id))
    } else {
        UserLookup.NotFound
    }
}

val user = findUser(123)
user match {
    case UserLookup.Found(u) => println("Found: " + u.name)
    case UserLookup.NotFound => println("User not found")
}
```

## Modeling Operations That Can Fail

Panther doesn't ship a built-in `Result` type either. The same enum pattern works for success/failure outcomes:

```panther
enum ParseResult {
    case Ok(value: int)
    case Err(message: string)
}

def isValidNumber(text: string): bool = text.length > 0

def toInt(text: string): int = text.length

def parseNumber(text: string): ParseResult = {
    if (isValidNumber(text)) {
        ParseResult.Ok(toInt(text))
    } else {
        ParseResult.Err("Invalid number format")
    }
}

val result = parseNumber("42")
result match {
    case ParseResult.Ok(num) => println("Number: " + string(num))
    case ParseResult.Err(msg) => println("Error: " + msg)
}
```

## Multiple Expressions

When a function has multiple statements, the last expression is returned. This example works in whole-number percentages since Panther has no `float`:

```panther
def calculateDiscount(price: int, percentage: int): int = {
    // Validation
    if (price <= 0) {
        0
    } else if (percentage < 0 || percentage > 100) {
        price
    } else {
        // Main logic - last expression is returned
        price - (price * percentage / 100)
    }
}
```

## Common Patterns

### Validation with Early Return

```panther
class Applicant(name: string, age: int, email: string)

enum ValidationResult {
    case Valid(user: Applicant)
    case Invalid(reason: string)
}

def validateUser(user: Applicant): ValidationResult = {
    if (user.name.length == 0) {
        ValidationResult.Invalid("Name is required")
    } else if (user.age < 0) {
        ValidationResult.Invalid("Age must be positive")
    } else if (user.email.length == 0) {
        ValidationResult.Invalid("Email is required")
    } else {
        ValidationResult.Valid(user)
    }
}
```

### Transformation

```panther
def transformData(input: int): int = {
    val doubled = input * 2
    val incremented = doubled + 1
    incremented
}

// Or as expression
def transformDataExpr(input: int): int = input * 2 + 1
```

### Computation with Multiple Steps

```panther
def calculateFinalPrice(basePrice: int, taxPercent: int, discountPercent: int): int = {
    val priceWithTax = basePrice + (basePrice * taxPercent / 100)
    val discountAmount = priceWithTax * discountPercent / 100
    val finalPrice = priceWithTax - discountAmount
    finalPrice
}
```
