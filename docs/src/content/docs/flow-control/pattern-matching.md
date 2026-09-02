---
title: Pattern Matching
description: Match expressions in Panther
---

Pattern matching provides a powerful way to check values against patterns and execute code based on which pattern matches.

## Match Expressions

The `match` expression checks a value against multiple patterns. Every pattern is introduced with `case`, and there is no way to combine several literals into one case — each value needs its own `case`:

```panther
val day = "Monday"

day match {
    case "Saturday" => println("Weekend")
    case "Sunday" => println("Weekend")
    case "Monday" => println("Weekday")
    case "Tuesday" => println("Weekday")
    case "Wednesday" => println("Weekday")
    case "Thursday" => println("Weekday")
    case "Friday" => println("Weekday")
}
```

## Match Syntax

Basic structure of a match expression:

<!-- panther-check: parse-only -->
```panther
value match {
    case pattern1 => expression1
    case pattern2 => expression2
    case pattern3 => expression3
}
```

## Literal Patterns

Match against specific values:

```panther
val number = 42

number match {
    case 0 => println("Zero")
    case 1 => println("One")
    case 42 => println("The answer!")
    case 100 => println("Century")
}
```

## Multiple Patterns

Panther's `match` has no `|` or comma syntax for matching several values with one case body. Write a separate `case` for each value instead:

```panther
val month = "June"

month match {
    case "December" => println("Winter")
    case "January" => println("Winter")
    case "February" => println("Winter")
    case "March" => println("Spring")
    case "April" => println("Spring")
    case "May" => println("Spring")
    case "June" => println("Summer")
    case "July" => println("Summer")
    case "August" => println("Summer")
    case "September" => println("Fall")
    case "October" => println("Fall")
    case "November" => println("Fall")
}
```

## Wildcard Pattern

The `_` wildcard matches any value:

```panther
val value = 99

value match {
    case 0 => println("Zero")
    case 1 => println("One")
    case _ => println("Some other number")
}
```

The wildcard is typically used as the last case to handle all remaining values.

## Match as an Expression

Match expressions return values:

```panther
val day = "Monday"

val dayType = day match {
    case "Saturday" => "Weekend"
    case "Sunday" => "Weekend"
    case "Monday" => "Weekday"
    case "Tuesday" => "Weekday"
    case "Wednesday" => "Weekday"
    case "Thursday" => "Weekday"
    case "Friday" => "Weekday"
    case _ => "Unknown"
}

println(dayType)
```

Use in calculations:

```panther
val grade = "B"

val points = grade match {
    case "A" => 4
    case "B" => 3
    case "C" => 2
    case "D" => 1
    case "F" => 0
    case _ => 0
}
```

## Block Expressions

Match cases can contain multiple statements:

```panther
def logSuccess(message: string): unit = println("LOG: " + message)
def logError(message: string): unit = println("ERR: " + message)

val status = "error"

status match {
    case "success" => {
        val message = "Operation completed"
        logSuccess(message)
        println(message)
    }
    case "error" => {
        val message = "Operation failed"
        logError(message)
        println(message)
    }
    case _ => {
        println("Unknown status")
    }
}
```

## Matching Numbers

Match against numeric values:

```panther
val age = 25

val category = age match {
    case 0 => "newborn"
    case 1 => "toddler"
    case 2 => "toddler"
    case 3 => "preschool"
    case 4 => "preschool"
    case 5 => "preschool"
    case _ => "other"
}
```

## Matching Strings

```panther
def startProcess(): unit = println("starting")
def stopProcess(): unit = println("stopping")
def printStatus(): unit = println("status: ok")

val command = "start"

command match {
    case "start" => startProcess()
    case "stop" => stopProcess()
    case "restart" => {
        stopProcess()
        startProcess()
    }
    case "status" => printStatus()
    case _ => println("Unknown command")
}
```

## Matching Types

Match based on type:

```panther
val value: any = 42

value match {
    case v: int => println("Integer: " + string(v))
    case v: string => println("String: " + v)
    case v: bool => println("Boolean: " + string(v))
    case _ => println("Other type")
}
```

## Destructuring Classes

Panther has no tuple literal syntax (`(x, y)`), but any class can be destructured the same way — a pattern names the class and binds its fields:

```panther
class Point(x: int, y: int)

val point = Point(10, 20)

point match {
    case Point(0, 0) => println("Origin")
    case Point(x, 0) => println("On X-axis at " + string(x))
    case Point(0, y) => println("On Y-axis at " + string(y))
    case Point(x, y) => println("Point at (" + string(x) + ", " + string(y) + ")")
}
```

## Matching Options

`Option` is not a built-in type, but you can define one and match on it like any other enum:

```panther
enum Option[T] {
    case Some(value: T)
    case None
}

val maybeValue: Option[int] = Option.Some(42)

maybeValue match {
    case Option.Some(value) => println("Found: " + string(value))
    case Option.None => println("No value")
}
```

Use in functions:

```panther
enum Option[T] {
    case Some(value: T)
    case None
}

def processOption(opt: Option[string]): string = {
    opt match {
        case Option.Some(s) => "Value: " + string(s)
        case Option.None => "No value provided"
    }
}
```

## Matching Results

The same approach works for a `Result`-shaped enum:

```panther
enum Result {
    case Ok(value: int)
    case Err(error: string)
}

val result: Result = Result.Ok(42)

result match {
    case Result.Ok(value) => println("Success: " + string(value))
    case Result.Err(error) => println("Error: " + error)
}
```

With error handling:

```panther
enum Result {
    case Ok(value: int)
    case Err(error: string)
}

def processResult(r: Result): int = {
    r match {
        case Result.Ok(n) => n * 2
        case Result.Err(msg) => {
            println(msg)
            0
        }
    }
}
```

## Guard Conditions

Panther's `match` does not support pattern guards (`case n if n < 0 => ...`) — a `case` is followed directly by `=>`, with no conditional clause in between. For value ranges like this, use an `if`/`else if` chain instead:

```panther
val number = 15

val description = if (number < 0) {
    "Negative"
} else if (number == 0) {
    "Zero"
} else if (number < 10) {
    "Small positive"
} else if (number < 100) {
    "Medium positive"
} else {
    "Large positive"
}

println(description)
```

## Nested Matching

Patterns can nest, including inside a class's fields:

```panther
enum Option[T] {
    case Some(value: T)
    case None
}

class Pair(first: Option[int], second: Option[int])

val pair = Pair(Option.Some(10), Option.Some(20))

pair match {
    case Pair(Option.Some(x), Option.Some(y)) => println("Both values: " + string(x) + ", " + string(y))
    case Pair(Option.Some(x), Option.None) => println("Only first: " + string(x))
    case Pair(Option.None, Option.Some(y)) => println("Only second: " + string(y))
    case Pair(Option.None, Option.None) => println("No values")
}
```

## Matching Enums

Match enum variants. Enum cases are declared one per `case` line (no trailing commas), and `match` is always written after the scrutinee — there is no `match (value) { ... }` prefix form:

```panther
enum Color {
    case Red
    case Green
    case Blue
}

val color = Color.Red

color match {
    case Color.Red => println("Red color")
    case Color.Green => println("Green color")
    case Color.Blue => println("Blue color")
}
```

## Matching Discriminated Unions

Panther has no `union` keyword — algebraic sum types with per-variant data are written as an `enum` whose cases carry parameters, just like `Color` above but with fields:

```panther
enum Shape {
    case Circle(radius: int)
    case Rectangle(width: int, height: int)
    case Triangle(base: int, height: int)
}

val shape = Shape.Circle(5)

val description = shape match {
    case Shape.Circle(r) => "circle with radius " + string(r)
    case Shape.Rectangle(w, h) => "rectangle " + string(w) + "x" + string(h)
    case Shape.Triangle(b, h) => "triangle with base " + string(b) + " and height " + string(h)
}

println(description)
```

## Exhaustiveness

Match expressions should handle all possible cases:

```panther
enum Option[T] {
    case Some(value: T)
    case None
}

val option: Option[int] = Option.Some(5)

// Good: all cases covered
val result = option match {
    case Option.Some(x) => x
    case Option.None => 0
}

val value = 1

// Good: wildcard catches all remaining cases
val category = value match {
    case 0 => "zero"
    case 1 => "one"
    case _ => "other"
}
```

## Common Patterns

### State Machine

```panther
var state = "idle"

while (state != "done") {
    state = state match {
        case "idle" => {
            println("Starting")
            "processing"
        }
        case "processing" => {
            println("Working")
            "complete"
        }
        case "complete" => {
            println("Finishing")
            "done"
        }
        case _ => "done"
    }
}
```

### Command Processing

```panther
def addItem(item: string): string = "added: " + item
def removeItem(item: string): string = "removed: " + item
def listItems(): string = "listing items"
def clearAll(): string = "cleared"

def executeCommand(cmd: string, args: Array[string]): string = {
    cmd match {
        case "add" => addItem(args(0))
        case "remove" => removeItem(args(0))
        case "list" => listItems()
        case "clear" => clearAll()
        case _ => "Unknown command: " + cmd
    }
}
```

### Error Code Translation

```panther
val errorCode = 2

val errorMessage = errorCode match {
    case 0 => "Success"
    case 1 => "File not found"
    case 2 => "Permission denied"
    case 3 => "Invalid input"
    case 4 => "Network error"
    case _ => "Unknown error: " + string(errorCode)
}
```

### Type-Based Dispatch

```panther
def processValue(value: any): string = {
    value match {
        case n: int => "Processing integer: " + string(n)
        case s: string => "Processing string: " + s
        case b: bool => "Processing boolean: " + string(b)
        case _ => "Unknown type"
    }
}
```
