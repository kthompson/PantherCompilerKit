---
title: Conditional Statements
description: If/else expressions in Panther
---

Conditional statements allow you to execute different code based on boolean conditions.

## If Expressions

The `if` statement evaluates a boolean condition:

```panther
val age = 18

if (age >= 18) {
    println("Adult")
}
```

## If-Else

Provide an alternative path with `else`:

```panther
val temperature = 75

if (temperature > 80) {
    println("It's hot!")
} else {
    println("It's comfortable")
}
```

## If-Else-If Chains

Chain multiple conditions together:

```panther
val score = 85

if (score >= 90) {
    println("Grade: A")
} else if (score >= 80) {
    println("Grade: B")
} else if (score >= 70) {
    println("Grade: C")
} else if (score >= 60) {
    println("Grade: D")
} else {
    println("Grade: F")
}
```

## If as an Expression

In Panther, `if` is an expression that returns a value:

```panther
val max = if (a > b) a else b

val status = if (isActive) {
    "active"
} else {
    "inactive"
}
```

This makes it similar to a ternary operator in other languages.

## Nested If Expressions

You can nest if expressions:

```panther
val x = 10
val y = 20

if (x > 0) {
    if (y > 0) {
        println("Both positive")
    } else {
        println("x positive, y not positive")
    }
} else {
    println("x not positive")
}
```

## Block Expressions

Each branch can contain multiple expressions:

```panther
val result = if (condition) {
    val temp = computeValue()
    val adjusted = temp * 2
    adjusted  // Last expression is the result
} else {
    val fallback = getDefault()
    fallback
}
```

## Comparison Operators

Use these operators in conditions:

```panther
val a = 10
val b = 20

// Equality
if (a == b) { println("Equal") }
if (a != b) { println("Not equal") }

// Comparison
if (a < b) { println("Less than") }
if (a <= b) { println("Less than or equal") }
if (a > b) { println("Greater than") }
if (a >= b) { println("Greater than or equal") }
```

## Logical Operators

Combine conditions with logical operators:

```panther
val age = 25
val hasLicense = true

// AND - both must be true
if (age >= 18 && hasLicense) {
    println("Can drive")
}

// OR - at least one must be true
if (age < 18 || !hasLicense) {
    println("Cannot drive")
}

// NOT - inverts the boolean
if (!hasLicense) {
    println("No license")
}
```

## Short-Circuit Evaluation

Logical operators use short-circuit evaluation:

```panther
// If first is false, second is not evaluated
if (denominator != 0 && numerator / denominator > 1) {
    println("Safe division")
}

// If first is true, second is not evaluated
if (value == null || value.length() > 0) {
    println("Valid or null")
}
```

## Common Patterns

### Range Checking

```panther
val value = 50

if (value >= 0 && value <= 100) {
    println("In range")
}
```

### Type of Classification

```panther
val number = -5

val classification = if (number > 0) {
    "positive"
} else if (number < 0) {
    "negative"
} else {
    "zero"
}
```

### Guard Conditions

```panther
def processValue(value: int): string = {
    if (value < 0) {
        "Invalid: negative"
    } else if (value > 100) {
        "Invalid: too large"
    } else {
        "Valid: " + value
    }
}
```

### Boolean Flags

```panther
val isValid = true
val isComplete = false

if (isValid && isComplete) {
    println("Ready to submit")
} else if (isValid && !isComplete) {
    println("Still working")
} else {
    println("Invalid")
}
```

## Expression Results

Since if is an expression, you can use it anywhere a value is expected:

```panther
// As a function argument
println(if (isDebug) "Debug mode" else "Production mode")

// In calculations
val discount = basePrice * if (isMember) 0.1 else 0
```

## Exhaustive Conditions

Ensure all cases are covered:

```panther
// Good: all cases covered
val category = if (age < 13) {
    "child"
} else if (age < 20) {
    "teenager"
} else if (age < 65) {
    "adult"
} else {
    "senior"
}

// Each branch must return the same type
val result: int = if (condition) {
    42
} else {
    0
}
```
