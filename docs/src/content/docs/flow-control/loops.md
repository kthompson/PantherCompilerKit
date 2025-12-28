---
title: Loops
description: While and for loops in Panther
---

Loops allow you to execute code repeatedly based on conditions or over collections.

## While Loops

Execute code while a condition is true:

```panther
var count = 0

while (count < 5) {
    println(count)
    count = count + 1
}
// Prints: 0, 1, 2, 3, 4
```

## While Loop Structure

The condition is checked before each iteration:

```panther
var i = 0
while (i < 3) {
    println("Iteration: " + i)
    i = i + 1
}
```

If the condition is initially false, the loop never executes:

```panther
var x = 10
while (x < 5) {
    println("This never prints")
}
```

## Infinite Loops

Be careful to ensure the condition eventually becomes false:

```panther
var running = true
var attempts = 0

while (running) {
    val input = readLine()
    
    if (input == "quit") {
        running = false
    }
    
    attempts = attempts + 1
    
    if (attempts > 100) {
        running = false
    }
}
```

## For Loops

Iterate over ranges and collections.

### Range-Based For Loops

Iterate from a start value to an end value:

```panther
// Inclusive range (0 to 5)
for (i in 0..5) {
    println(i)
}
// Prints: 0, 1, 2, 3, 4, 5

// Exclusive range (0 to 4)
for (i in 0..<5) {
    println(i)
}
// Prints: 0, 1, 2, 3, 4
```

### Step Ranges

Iterate with a custom step:

```panther
// Count by twos
for (i in 0..10 step 2) {
    println(i)
}
// Prints: 0, 2, 4, 6, 8, 10

// Count by fives
for (i in 0..50 step 5) {
    println(i)
}
// Prints: 0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50
```

### Descending Ranges

Iterate backwards:

```panther
for (i in 10 downTo 0) {
    println(i)
}
// Prints: 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0

for (i in 5 downTo 1 step 2) {
    println(i)
}
// Prints: 5, 3, 1
```

## Collection Iteration

Iterate over arrays and lists:

```panther
val fruits = ["apple", "banana", "cherry"]

for (fruit in fruits) {
    println(fruit)
}
// Prints: apple, banana, cherry
```

### With Index

Access both the element and its index:

```panther
val colors = ["red", "green", "blue"]

for ((index, color) in colors.withIndex()) {
    println(index + ": " + color)
}
// Prints: 0: red, 1: green, 2: blue
```

## Nested Loops

Loops can be nested inside other loops:

```panther
for (i in 1..3) {
    for (j in 1..3) {
        println("i=" + i + ", j=" + j)
    }
}
```

Create multiplication tables:

```panther
for (i in 1..10) {
    for (j in 1..10) {
        val product = i * j
        print(product + "\t")
    }
    println("")
}
```

## Loop Patterns

### Accumulation

Sum values in a loop:

```panther
var sum = 0
for (i in 1..10) {
    sum = sum + i
}
println("Sum: " + sum)  // Sum: 55
```

### Finding Maximum

```panther
val numbers = [3, 7, 2, 9, 1, 5]
var max = numbers[0]

for (num in numbers) {
    if (num > max) {
        max = num
    }
}
println("Maximum: " + max)  // Maximum: 9
```

### Counting

Count elements that match a condition:

```panther
val values = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
var evenCount = 0

for (value in values) {
    if (value % 2 == 0) {
        evenCount = evenCount + 1
    }
}
println("Even numbers: " + evenCount)  // Even numbers: 5
```

### Building Collections

Create a new collection based on existing data:

```panther
val numbers = [1, 2, 3, 4, 5]
val doubled = []

for (num in numbers) {
    doubled.add(num * 2)
}
// doubled is [2, 4, 6, 8, 10]
```

### Filtering

Select elements that match criteria:

```panther
val values = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
val evens = []

for (value in values) {
    if (value % 2 == 0) {
        evens.add(value)
    }
}
// evens is [2, 4, 6, 8, 10]
```

## While vs For

**Use while when:**
- The number of iterations is unknown
- Looping until a condition changes
- Reading input until a sentinel value

```panther
var input = ""
while (input != "quit") {
    input = readLine()
    processInput(input)
}
```

**Use for when:**
- Iterating a specific number of times
- Processing collections
- Working with ranges

```panther
for (i in 0..10) {
    processValue(i)
}
```

## Common Loop Patterns

### Countdown

```panther
for (i in 10 downTo 1) {
    println(i)
}
println("Liftoff!")
```

### Processing Pairs

```panther
val names = ["Alice", "Bob", "Charlie"]
val scores = [95, 87, 92]

for (i in 0..<names.length()) {
    println(names[i] + ": " + scores[i])
}
```

### Sliding Window

```panther
val numbers = [1, 2, 3, 4, 5]

for (i in 0..<numbers.length() - 1) {
    val current = numbers[i]
    val next = numbers[i + 1]
    println(current + " -> " + next)
}
```

### Grid Processing

```panther
val rows = 3
val cols = 4

for (row in 0..<rows) {
    for (col in 0..<cols) {
        val index = row * cols + col
        println("Cell[" + row + "][" + col + "] = " + index)
    }
}
```

## Loop Variables

Loop variables are scoped to the loop:

```panther
for (i in 0..5) {
    println(i)
}
// i is not accessible here

for (item in collection) {
    println(item)
}
// item is not accessible here
```

## Empty Loops

Loops with no iterations are valid:

```panther
for (i in 5..<5) {
    println("Never executes")
}

var x = 10
while (x < 5) {
    println("Never executes")
}
```
