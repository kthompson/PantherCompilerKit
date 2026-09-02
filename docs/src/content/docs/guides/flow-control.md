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

In Panther, `if` is an expression that produces a value:

```panther
val a = 3
val b = 7
val largest = if (a > b) a else b

val isActive = true
val status = if (isActive) {
  "active"
} else {
  "inactive"
}

println(string(largest))
println(status)
```

## While Loops

Execute code repeatedly while a condition is true:

```panther
var count = 0

while (count < 5) {
  println(string(count))
  count = count + 1
}
```

### Terminating a Loop

Panther has no `break` (see below), so a loop that needs an early exit uses a
flag in its condition:

```panther
var running = true
var attempts = 0

while (running) {
  attempts = attempts + 1
  if (attempts >= 3) {
    running = false
  }
}

println("stopped after " + string(attempts) + " attempts")
```

## For Loops

A `for` loop counts over an inclusive numeric range, written with `<-` and
`to`:

```panther
// Iterate from 0 to 4 inclusive
for (i <- 0 to 4) {
  println(string(i))
}
```

This is the only form. There is no exclusive range, no `step`, no `downTo`,
and no syntax for iterating a collection directly. To count downwards or by a
stride, use a `while` loop:

```panther
var i = 10
while (i >= 0) {
  println(string(i))
  i = i - 2
}
```

### Walking an Array

Arrays are traversed by index. `length` is a field, so it takes no
parentheses:

```panther
val fruits = new Array[string](3)
fruits(0) = "apple"
fruits(1) = "banana"
fruits(2) = "cherry"

for (i <- 0 to (fruits.length - 1)) {
  println(fruits(i))
}
```

Note that Panther has no array literal syntax — `["apple", "banana"]` does not
parse. Arrays are allocated with a size and filled by index.

## Break and Continue

`break` and `continue` are reserved words and they parse, but the binder does
not implement them: a program that uses one currently crashes the compiler
rather than producing a diagnostic. Until they are implemented, restructure the
loop.

Instead of `break`, use a flag in the loop condition, as shown above. Instead
of `continue`, invert the condition so the skipped case simply does nothing:

```panther
for (i <- 0 to 10) {
  if (i % 2 != 0) {
    println(string(i))  // Prints 1, 3, 5, 7, 9
  }
}
```

## Pattern Matching

`match` is written after the value being matched, and every branch starts with
`case`:

```panther
val day = "Monday"

val dayType = day match {
  case "Saturday" => "Weekend"
  case "Sunday"   => "Weekend"
  case _          => "Weekday"
}

println(dayType)  // Prints: Weekday
```

Each pattern needs its own `case` — there is no comma or `|` alternation, and
no `if` guards on a case.

### Matching Values

```panther
val number = 42

number match {
  case 0  => println("Zero")
  case 1  => println("One")
  case 42 => println("The answer!")
  case _  => println("Some other number")
}
```

### Match as Expression

`match` produces a value, so it can be assigned directly:

```panther
val value = 1

val result = value match {
  case 0 => "none"
  case 1 => "one"
  case _ => "many"
}

println(result)  // Prints: one
```

## Guard Clauses

Panther has no `return`, so a function cannot exit early. Express the same
logic as a chain of conditions, or as a `match`:

```panther
def processValue(value: int): string =
  if (value < 0) "Negative"
  else if (value == 0) "Zero"
  else "Positive"

println(processValue(-4))
```

## Conditional Expressions

### Ternary-Style Operations

Use `if-else` expressions for simple conditions:

```panther
val isValid = true
val message = if (isValid) "Success" else "Error"

val x = -3
val abs = if (x >= 0) x else -x

println(message + " " + string(abs))
```

## Best Practices

1. **Prefer expressions over statements** - Use `if` and `match` for their values
2. **Avoid deep nesting** - Extract functions rather than nesting conditions
3. **Use pattern matching** - It's more readable than complex if-else chains
4. **Keep loop bodies simple** - Extract complex logic into functions
5. **Be careful with infinite loops** - Always ensure termination conditions

## Common Patterns

### Finding Maximum

```panther
val numbers = new Array[int](4)
numbers(0) = 3
numbers(1) = 9
numbers(2) = 4
numbers(3) = 1

var largest = numbers(0)
for (i <- 1 to (numbers.length - 1)) {
  if (numbers(i) > largest) {
    largest = numbers(i)
  }
}

println(string(largest))  // Prints: 9
```

### Validation

```panther
def validateAge(age: int): bool =
  if (age < 0) false
  else if (age > 150) false
  else true

println(string(validateAge(42)))
```

### State Machines

```panther
var state = "start"

while (state != "end") {
  state = state match {
    case "start"      => "processing"
    case "processing" => "complete"
    case _            => "end"
  }
}

println(state)  // Prints: end
```

## Next Steps

- [Functions](/guides/functions) - Learn about function definitions and usage
- [Data Types](/guides/data-types) - Explore Panther's type system
