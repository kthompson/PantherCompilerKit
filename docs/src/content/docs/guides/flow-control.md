---
title: Flow Control
description: Master conditional logic, loops, and control flow in Panther
---

Control flow structures allow you to control the execution path of your Panther programs based on conditions and repetition.

## If Expressions

The `if` statement evaluates a boolean condition and executes code accordingly:

```panther
val age = 18

if (age >= 18) {
    println("Adult")
}
```

### If-Else

Provide an alternative path with `else`:

```panther
val temperature = 75

if (temperature > 80) {
    println("It's hot!")
} else {
    println("It's comfortable")
}
```

### If-Else If-Else

Chain multiple conditions:

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

### If as Expression

In Panther, `if` is an expression that returns a value:

```panther
val max = if (a > b) a else b

val status = if (isActive) {
    "active"
} else {
    "inactive"
}
```

## While Loops

Execute code repeatedly while a condition is true:

```panther
var count = 0

while (count < 5) {
    println(count)
    count = count + 1
}
```

### Infinite Loops

Be careful to ensure the condition eventually becomes false:

```panther
var running = true

while (running) {
    val input = readLine()
    if (input == "quit") {
        running = false
    }
}
```

## For Loops

Iterate over ranges and collections:

### Range-Based For Loops

```panther
// Iterate from 0 to 4 (inclusive)
for (i in 0..5) {
    println(i)
}

// Iterate from 0 to 4 (exclusive)
for (i in 0..<5) {
    println(i)
}

// Iterate with step
for (i in 0..10 step 2) {
    println(i)  // Prints 0, 2, 4, 6, 8, 10
}

// Iterate backwards
for (i in 10 downTo 0) {
    println(i)
}
```

### Collection Iteration

```panther
val fruits = ["apple", "banana", "cherry"]

for (fruit in fruits) {
    println(fruit)
}
```

## Break and Continue

Control loop execution flow:

### Break

Exit a loop early:

```panther
for (i in 0..10) {
    if (i == 5) {
        break  // Exit the loop when i is 5
    }
    println(i)  // Prints 0, 1, 2, 3, 4
}
```

### Continue

Skip to the next iteration:

```panther
for (i in 0..10) {
    if (i % 2 == 0) {
        continue  // Skip even numbers
    }
    println(i)  // Prints 1, 3, 5, 7, 9
}
```

## Pattern Matching

Use `match` expressions for powerful pattern matching:

```panther
val day = "Monday"

val dayType = match (day) {
    "Saturday", "Sunday" => "Weekend"
    "Monday", "Tuesday", "Wednesday", "Thursday", "Friday" => "Weekday"
    _ => "Unknown"
}

println(dayType)  // Prints: Weekday
```

### Matching Values

```panther
val number = 42

match (number) {
    0 => println("Zero")
    1 => println("One")
    42 => println("The answer!")
    _ => println("Some other number")
}
```

### Match as Expression

```panther
val result = match (value) {
    0 => "none"
    1 => "one"
    _ => "many"
}
```

## Guard Clauses

Use early returns to simplify complex logic:

```panther
fun processValue(value: Int): String {
    if (value < 0) {
        return "Negative"
    }
    
    if (value == 0) {
        return "Zero"
    }
    
    return "Positive"
}
```

## Conditional Expressions

### Ternary-Style Operations

Use `if-else` expressions for simple conditions:

```panther
val message = if (isValid) "Success" else "Error"

val abs = if (x >= 0) x else -x
```

## Best Practices

1. **Prefer expressions over statements** - Use `if` as an expression when possible
2. **Avoid deep nesting** - Use early returns or extract functions
3. **Use pattern matching** - It's more readable than complex if-else chains
4. **Keep loop bodies simple** - Extract complex logic into functions
5. **Be careful with infinite loops** - Always ensure termination conditions

## Common Patterns

### Finding Maximum

```panther
var max = numbers[0]
for (num in numbers) {
    if (num > max) {
        max = num
    }
}
```

### Validation

```panther
fun validateAge(age: Int): Bool {
    if (age < 0) {
        return false
    }
    
    if (age > 150) {
        return false
    }
    
    return true
}
```

### State Machines

```panther
var state = "start"

while (state != "end") {
    match (state) {
        "start" => {
            state = "processing"
        }
        "processing" => {
            state = "complete"
        }
        "complete" => {
            state = "end"
        }
    }
}
```

## Next Steps

- [Functions](/guides/functions) - Learn about function definitions and usage
- [Data Types](/guides/data-types) - Explore Panther's type system
