---
title: Option and Result
description: Handling optional and fallible values
---

# Option and Result

The standard library models values that may be missing with `Option` and operations that can fail with `Result`. The compiler includes both automatically, so no import is needed.

## Option

The standard library's `Option[T]` represents values that may or may not exist, with `None` and `Some` cases:

```panther
val some: Option[int] = Option.Some(42)
val none: Option[int] = Option.None

// Pattern matching
val result = some match {
  case Option.Some(value) => value
  case Option.None => 0
}

println(string(result))
```

## Result

The standard library's `Result[ErrorType, ValueType]` represents operations that can succeed or fail:

```panther
def divide(a: int, b: int): Result[string, int] = {
  if (b == 0) {
    Result.Error("Division by zero")
  } else {
    Result.Success(a / b)
  }
}

val result = divide(10, 2)

val message = result match {
  case Result.Success(value) => "Result: " + string(value)
  case Result.Error(error) => "Error: " + error
}

println(message)
```
