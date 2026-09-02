---
title: Higher-Order Functions
description: Where Panther stands on functions as values, and what to use instead
---

A higher-order function is one that takes another function as an argument or
returns one. **Panther does not support this yet.** There is no way to write a
parameter whose type is a function, and there are no anonymous functions.

This page records the intended design and, more usefully, shows what to write
today instead.

## What is not implemented

Neither of these parses:

<!-- panther-check: skip reason="function-typed parameters are not implemented" -->
```panther
def applyTwice(f: (int) -> int, x: int): int = f(f(x))
```

<!-- panther-check: skip reason="lambdas are not implemented" -->
```panther
val double = (x: int) => x * 2
```

The parser rejects the first at the parameter's opening parenthesis, and the
second at `=>`. Because function values do not exist, none of the usual
building blocks — `map`, `filter`, `reduce`, function composition, currying,
partial application, closures — can be expressed. Nested functions are not
supported either: a `def` cannot appear inside another function's body.

## What to write instead

### Named functions at the top level

Give the operation a name and call it directly. This covers most cases where a
lambda would otherwise be passed inline:

```panther
def double(x: int): int = x * 2

def applyToRange(limit: int): unit = {
  for (i <- 0 to limit) {
    println(string(double(i)))
  }
}

applyToRange(4)
```

### Generic functions

Generics do work, so a function can be written once and used at several types:

```panther
def identity[T](x: T): T = x

println(string(identity(42)))
println(identity("hello"))
```

### Transforming an array

Where another language would write `map`, walk the array by index and build a
new one:

```panther
val source = new Array[int](4)
source(0) = 1
source(1) = 2
source(2) = 3
source(3) = 4

val doubled = new Array[int](source.length)
for (i <- 0 to (source.length - 1)) {
  doubled(i) = source(i) * 2
}

for (i <- 0 to (doubled.length - 1)) {
  println(string(doubled(i)))
}
```

### Filtering

Without function values, filtering is a two-pass job: count the matches, then
allocate and fill.

```panther
val values = new Array[int](5)
values(0) = 1
values(1) = 2
values(2) = 3
values(3) = 4
values(4) = 5

var matches = 0
for (i <- 0 to (values.length - 1)) {
  if (values(i) % 2 == 0) {
    matches = matches + 1
  }
}

val evens = new Array[int](matches)
var next = 0
for (i <- 0 to (values.length - 1)) {
  if (values(i) % 2 == 0) {
    evens(next) = values(i)
    next = next + 1
  }
}

println("found " + string(matches) + " even numbers")
```

### Reducing

An accumulator in a loop replaces `reduce`:

```panther
val numbers = new Array[int](4)
numbers(0) = 10
numbers(1) = 20
numbers(2) = 30
numbers(3) = 40

var total = 0
for (i <- 0 to (numbers.length - 1)) {
  total = total + numbers(i)
}

println("sum: " + string(total))
```

### Choosing behaviour at runtime

Where another language would pass a strategy function, use an `enum` and
`match`. This is the closest thing Panther has to dispatching on behaviour:

```panther
enum Operation {
  case Add
  case Multiply
  case Max
}

def apply(op: Operation, a: int, b: int): int = op match {
  case Operation.Add      => a + b
  case Operation.Multiply => a * b
  case Operation.Max      => if (a > b) a else b
}

println(string(apply(Operation.Add, 3, 4)))
println(string(apply(Operation.Multiply, 3, 4)))
println(string(apply(Operation.Max, 3, 4)))
```

The enum is closed, so unlike a function parameter it cannot be extended by a
caller — but it is checked exhaustively and it works today.

## Next Steps

- [Defining Functions](defining-functions) - Function declaration syntax
- [Parameters](parameters) - Passing values into functions
- [Return Values](return-values) - What a function produces
