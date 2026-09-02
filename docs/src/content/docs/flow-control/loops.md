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
    println("Iteration: " + string(i))
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
    // ... do some work here ...

    attempts = attempts + 1

    // Always include a way out, in case the expected condition never arrives
    if (attempts > 100) {
        running = false
    }
}

println("Stopped after " + string(attempts) + " attempts")
```

## For Loops

Panther's `for` loop counts a variable across an integer range. There is no `for (x in xs)` collection syntax — to visit every element of an array, loop over its indices instead.

### Range-Based For Loops

The loop variable counts from a start value up to (and including) an end value:

```panther
for (i <- 0 to 5) {
    println(string(i))
}
// Prints: 0, 1, 2, 3, 4, 5
```

The range is always inclusive on both ends, and always counts upward by one — there is no `step`, `downTo`, or exclusive-range syntax. To count downward or skip values, use a `while` loop instead (see below).

## Collection Iteration

Arrays don't support a `for (x in xs)` form. Iterate by index instead, using `.length` for the bound:

```panther
val fruits = new Array[string](3)
fruits(0) = "apple"
fruits(1) = "banana"
fruits(2) = "cherry"

for (i <- 0 to (fruits.length - 1)) {
    println(fruits(i))
}
// Prints: apple, banana, cherry
```

### With Index

Since you're already looping by index, the index is available for free — no separate `withIndex()` call is needed:

```panther
val colors = new Array[string](3)
colors(0) = "red"
colors(1) = "green"
colors(2) = "blue"

for (index <- 0 to (colors.length - 1)) {
    println(string(index) + ": " + colors(index))
}
// Prints: 0: red, 1: green, 2: blue
```

## Nested Loops

Loops can be nested inside other loops:

```panther
for (i <- 1 to 3) {
    for (j <- 1 to 3) {
        println("i=" + string(i) + ", j=" + string(j))
    }
}
```

Create multiplication tables:

```panther
for (i <- 1 to 10) {
    for (j <- 1 to 10) {
        val product = i * j
        print(string(product) + "\t")
    }
    println("")
}
```

## Loop Patterns

### Accumulation

Sum values in a loop:

```panther
var sum = 0
for (i <- 1 to 10) {
    sum = sum + i
}
println("Sum: " + string(sum))  // Sum: 55
```

### Finding Maximum

```panther
val numbers = new Array[int](6)
numbers(0) = 3
numbers(1) = 7
numbers(2) = 2
numbers(3) = 9
numbers(4) = 1
numbers(5) = 5

var max = numbers(0)

for (i <- 0 to (numbers.length - 1)) {
    if (numbers(i) > max) {
        max = numbers(i)
    }
}
println("Maximum: " + string(max))  // Maximum: 9
```

### Counting

Count elements that match a condition:

```panther
val values = new Array[int](10)
for (i <- 0 to (values.length - 1)) {
    values(i) = i + 1
}

var evenCount = 0
for (i <- 0 to (values.length - 1)) {
    if (values(i) % 2 == 0) {
        evenCount = evenCount + 1
    }
}
println("Even numbers: " + string(evenCount))  // Even numbers: 5
```

### Building Collections

Arrays are fixed-size, so "building" one means allocating a same-sized array up front and filling it in a loop:

```panther
val numbers = new Array[int](5)
for (i <- 0 to (numbers.length - 1)) {
    numbers(i) = i + 1
}

val doubled = new Array[int](numbers.length)
for (i <- 0 to (numbers.length - 1)) {
    doubled(i) = numbers(i) * 2
}
// doubled is [2, 4, 6, 8, 10]
```

### Filtering

To select only matching elements, count the matches first, then allocate a result array of exactly that size:

```panther
val values = new Array[int](10)
for (i <- 0 to (values.length - 1)) {
    values(i) = i + 1
}

var evenCount = 0
for (i <- 0 to (values.length - 1)) {
    if (values(i) % 2 == 0) {
        evenCount = evenCount + 1
    }
}

val evens = new Array[int](evenCount)
var evenIndex = 0
for (i <- 0 to (values.length - 1)) {
    if (values(i) % 2 == 0) {
        evens(evenIndex) = values(i)
        evenIndex = evenIndex + 1
    }
}
// evens is [2, 4, 6, 8, 10]
```

## While vs For

**Use while when:**
- The number of iterations is unknown
- Looping until a condition changes
- Counting downward or by a custom step (`for` only counts up by one)

```panther
var count = 10
while (count > 0) {
    println(string(count))
    count = count - 1
}
println("Liftoff!")
```

**Use for when:**
- Iterating a specific number of times
- Processing an array by index
- Working with a simple ascending range

```panther
def processValue(value: int): unit = println(string(value))

for (i <- 0 to 10) {
    processValue(i)
}
```

## Common Loop Patterns

### Countdown

`for` only counts upward, so a countdown uses `while`:

```panther
var i = 10
while (i >= 1) {
    println(string(i))
    i = i - 1
}
println("Liftoff!")
```

### Processing Pairs

```panther
val names = new Array[string](3)
names(0) = "Alice"
names(1) = "Bob"
names(2) = "Charlie"

val scores = new Array[int](3)
scores(0) = 95
scores(1) = 87
scores(2) = 92

for (i <- 0 to (names.length - 1)) {
    println(names(i) + ": " + string(scores(i)))
}
```

### Sliding Window

```panther
val numbers = new Array[int](5)
for (i <- 0 to (numbers.length - 1)) {
    numbers(i) = i + 1
}

for (i <- 0 to (numbers.length - 2)) {
    val current = numbers(i)
    val next = numbers(i + 1)
    println(string(current) + " -> " + string(next))
}
```

### Grid Processing

```panther
val rows = 3
val cols = 4

for (row <- 0 to (rows - 1)) {
    for (col <- 0 to (cols - 1)) {
        val index = row * cols + col
        println("Cell[" + string(row) + "][" + string(col) + "] = " + string(index))
    }
}
```

## Loop Variables

Loop variables are scoped to the loop:

```panther
for (i <- 0 to 5) {
    println(string(i))
}
// i is not accessible here
```

## Empty Loops

Loops with no iterations are valid — a range that starts above where it ends simply never runs:

```panther
for (i <- 5 to 4) {
    println("Never executes")
}

var x = 10
while (x < 5) {
    println("Never executes")
}
```
