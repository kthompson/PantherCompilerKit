---
title: Pattern Matching
description: Match expressions in Panther
---

Pattern matching provides a powerful way to check values against patterns and execute code based on which pattern matches.

## Match Expressions

The `match` expression checks a value against multiple patterns:

```panther
val day = "Monday"

day match {
    "Saturday", "Sunday" => println("Weekend")
    "Monday", "Tuesday", "Wednesday", "Thursday", "Friday" => println("Weekday")
}
```

## Match Syntax

Basic structure of a match expression:

```panther
value match {
    pattern1 => expression1
    pattern2 => expression2
    pattern3 => expression3
}
```

## Literal Patterns

Match against specific values:

```panther
val number = 42

number match {
    0 => println("Zero")
    1 => println("One")
    42 => println("The answer!")
    100 => println("Century")
}
```

## Multiple Patterns

Match multiple values with a single case:

```panther
val month = "June"

month match {
    "December", "January", "February" => println("Winter")
    "March", "April", "May" => println("Spring")
    "June", "July", "August" => println("Summer")
    "September", "October", "November" => println("Fall")
}
```

## Wildcard Pattern

The `_` wildcard matches any value:

```panther
val value = 99

value match {
    0 => println("Zero")
    1 => println("One")
    _ => println("Some other number")
}
```

The wildcard is typically used as the last case to handle all remaining values.

## Match as an Expression

Match expressions return values:

```panther
val dayType = day match {
    "Saturday", "Sunday" => "Weekend"
    "Monday", "Tuesday", "Wednesday", "Thursday", "Friday" => "Weekday"
    _ => "Unknown"
}

println(dayType)
```

Use in calculations:

```panther
val points = grade match {
    "A" => 4
    "B" => 3
    "C" => 2
    "D" => 1
    "F" => 0
    _ => 0
}
```

## Block Expressions

Match cases can contain multiple statements:

```panther
val status = "error"

status match {
    "success" => {
        val message = "Operation completed"
        logSuccess(message)
        println(message)
    }
    "error" => {
        val message = "Operation failed"
        logError(message)
        println(message)
    }
    _ => {
        println("Unknown status")
    }
}
```

## Matching Numbers

Match against numeric values:

```panther
val age = 25

val category = age match {
    0 => "newborn"
    1, 2 => "toddler"
    3, 4, 5 => "preschool"
    _ => "other"
}
```

## Matching Strings

```panther
val command = "start"

command match {
    "start" => startProcess()
    "stop" => stopProcess()
    "restart" => {
        stopProcess()
        startProcess()
    }
    "status" => printStatus()
    _ => println("Unknown command")
}
```

## Matching Types

Match based on type:

```panther
val value: Any = 42

value match {
    v: int => println("Integer: " + v)
    v: string => println("String: " + v)
    v: bool => println("Boolean: " + v)
    _ => println("Other type")
}
```

## Destructuring Tuples

Extract values from tuples:

```panther
val point = (10, 20)

point match {
    (0, 0) => println("Origin")
    (x, 0) => println("On X-axis at " + x)
    (0, y) => println("On Y-axis at " + y)
    (x, y) => println("Point at (" + x + ", " + y + ")")
}
```

## Matching Options

Handle optional values:

```panther
val maybeValue: Option<int> = Some(42)

maybeValue match {
    Some(value) => println("Found: " + value)
    None => println("No value")
}
```

Use in functions:

```panther
def processOption(opt: Option<string>): string = {
    opt match {
        Some(s) => "Value: " + s
        None => "No value provided"
    }
}
```

## Matching Results

Handle success or failure:

```panther
val result: Result<int, string> = Ok(42)

result match {
    Ok(value) => println("Success: " + value)
    Err(error) => println("Error: " + error)
}
```

With error handling:

```panther
def processResult(r: Result<int, string>): int = {
    r match {
        Ok(n) => n * 2
        Err(msg) => {
            logError(msg)
            0
        }
    }
}
```

## Guard Conditions

Add conditions to patterns:

```panther
val number = 15

number match {
    n if n < 0 => println("Negative")
    n if n == 0 => println("Zero")
    n if n < 10 => println("Small positive")
    n if n < 100 => println("Medium positive")
    _ => println("Large positive")
}
```

## Nested Matching

Match expressions can be nested:

```panther
val pair = (Some(10), Some(20))

pair match {
    (Some(x), Some(y)) => println("Both values: " + x + ", " + y)
    (Some(x), None) => println("Only first: " + x)
    (None, Some(y)) => println("Only second: " + y)
    (None, None) => println("No values")
}
```

## Matching Enums

Match enum variants:

```panther
enum Color {
    Red,
    Green,
    Blue
}

val color = Color.Red

match (color) {
    Color.Red => println("Red color")
    Color.Green => println("Green color")
    Color.Blue => println("Blue color")
}
```

## Matching Discriminated Unions

Match on union variants:

```panther
union Shape {
    Circle(radius: float),
    Rectangle(width: float, height: float),
    Triangle(base: float, height: float)
}

val shape = Shape.Circle(5.0)

val area = match (shape) {
    Circle(r) => 3.14159 * r * r
    Rectangle(w, h) => w * h
    Triangle(b, h) => 0.5 * b * h
}
```

## Exhaustiveness

Match expressions should handle all possible cases:

```panther
// Good: all cases covered
val result = option match {
    Some(x) => x
    None => 0
}

// Good: wildcard catches all remaining cases
val category = value match {
    0 => "zero"
    1 => "one"
    _ => "other"
}
```

## Common Patterns

### State Machine

```panther
var state = "idle"

while (state != "done") {
    state = state match {
        "idle" => {
            println("Starting")
            "processing"
        }
        "processing" => {
            println("Working")
            "complete"
        }
        "complete" => {
            println("Finishing")
            "done"
        }
        _ => "done"
    }
}
```

### Command Processing

```panther
def executeCommand(cmd: string, args: Array<string>): string = {
    cmd match {
        "add" => addItem(args[0])
        "remove" => removeItem(args[0])
        "list" => listItems()
        "clear" => clearAll()
        _ => "Unknown command: " + cmd
    }
}
```

### Error Code Translation

```panther
val errorMessage = errorCode match {
    0 => "Success"
    1 => "File not found"
    2 => "Permission denied"
    3 => "Invalid input"
    4 => "Network error"
    _ => "Unknown error: " + errorCode
}
```

### Type-Based Dispatch

```panther
def processValue(value: Any): string = {
    value match {
        n: int => "Processing integer: " + n
        s: string => "Processing string: " + s
        b: bool => "Processing boolean: " + b
        _ => "Unknown type"
    }
}
```
