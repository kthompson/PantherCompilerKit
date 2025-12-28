---
title: Higher-Order Functions
description: Functions as first-class values in Panther
---

Higher-order functions are functions that take other functions as parameters or return functions as results. This enables powerful abstraction and code reuse.

## Functions as Parameters

Pass functions to other functions:

```panther
def applyOperation(x: int, y: int, op: (int, int) => int): int = {
    op(x, y)
}

def add(a: int, b: int): int = a + b
def multiply(a: int, b: int): int = a * b

val sum = applyOperation(5, 3, add)       // 8
val product = applyOperation(5, 3, multiply)  // 15
```

## Function Type Syntax

Function types are written as `(ParamTypes) => ReturnType`:

```panther
// Takes int, returns int
val f: (int) => int

// Takes two ints, returns int
val g: (int, int) => int

// Takes string, returns bool
val h: (string) => bool

// Takes no parameters, returns unit
val i: () => unit
```

## Lambda Expressions

Create anonymous functions inline:

```panther
val double = (x: int) => x * 2

val sum = (a: int, b: int) => a + b

val greet = (name: string) => "Hello, " + name
```

Use lambdas with higher-order functions:

```panther
val result = applyOperation(5, 3, (a: int, b: int) => a * a + b * b)
```

## Type Inference in Lambdas

Parameter types can often be inferred:

```panther
def applyTwice(f: (int) => int, x: int): int = {
    f(f(x))
}

// Type inferred from context
val result = applyTwice((x) => x + 1, 5)  // 7
```

## Returning Functions

Functions can return other functions:

```panther
def makeMultiplier(factor: int): (int) => int = {
    (x: int) => x * factor
}

val double = makeMultiplier(2)
val triple = makeMultiplier(3)

println(double(5))  // 10
println(triple(5))  // 15
```

## Closures

Returned functions can capture variables from their enclosing scope:

```panther
def makeCounter(): () => int = {
    var count = 0
    
    () => {
        count = count + 1
        count
    }
}

val counter = makeCounter()
println(counter())  // 1
println(counter())  // 2
println(counter())  // 3
```

## Common Higher-Order Functions

### Map

Transform each element of a collection:

```panther
def map<T, R>(list: List<T>, f: (T) => R): List<R> = {
    val result = []
    for (item in list) {
        result.add(f(item))
    }
    result
}

val numbers = [1, 2, 3, 4, 5]
val doubled = map(numbers, (x) => x * 2)  // [2, 4, 6, 8, 10]
```

### Filter

Select elements that match a predicate:

```panther
def filter<T>(list: List<T>, predicate: (T) => bool): List<T> = {
    val result = []
    for (item in list) {
        if (predicate(item)) {
            result.add(item)
        }
    }
    result
}

val numbers = [1, 2, 3, 4, 5, 6]
val evens = filter(numbers, (x) => x % 2 == 0)  // [2, 4, 6]
```

### Reduce

Combine all elements into a single value:

```panther
def reduce<T>(list: List<T>, initial: T, f: (T, T) => T): T = {
    var result = initial
    for (item in list) {
        result = f(result, item)
    }
    result
}

val numbers = [1, 2, 3, 4, 5]
val sum = reduce(numbers, 0, (acc, x) => acc + x)  // 15
```

## Function Composition

Combine functions to create new functions:

```panther
def compose<A, B, C>(f: (B) => C, g: (A) => B): (A) => C = {
    (x: A) => f(g(x))
}

val addOne = (x: int) => x + 1
val double = (x: int) => x * 2

val addOneThenDouble = compose(double, addOne)
println(addOneThenDouble(5))  // 12 (5 + 1 = 6, 6 * 2 = 12)
```

## Currying

Transform a multi-parameter function into a series of single-parameter functions:

```panther
def add(a: int): (int) => int = {
    (b: int) => a + b
}

val add5 = add(5)
println(add5(3))  // 8
println(add5(10)) // 15
```

## Partial Application

Create new functions by fixing some arguments:

```panther
def makeAdder(x: int): (int) => int = {
    (y: int) => x + y
}

def makeMultiplier(factor: int): (int) => int = {
    (n: int) => n * factor
}

val add10 = makeAdder(10)
val triple = makeMultiplier(3)

println(add10(5))   // 15
println(triple(4))  // 12
```

## Predicates

Functions that return boolean values:

```panther
def any<T>(list: List<T>, predicate: (T) => bool): bool = {
    for (item in list) {
        if (predicate(item)) {
            true  // Found a match
        }
    }
    false  // No matches found
}

def all<T>(list: List<T>, predicate: (T) => bool): bool = {
    for (item in list) {
        if (!predicate(item)) {
            false  // Found non-match
        }
    }
    true  // All match
}

val numbers = [1, 2, 3, 4, 5]
val hasEven = any(numbers, (x) => x % 2 == 0)    // true
val allPositive = all(numbers, (x) => x > 0)     // true
```

## Common Patterns

### Chaining Operations

```panther
val result = numbers
    .map((x) => x * 2)
    .filter((x) => x > 10)
    .reduce(0, (acc, x) => acc + x)
```

### Event Handlers

```panther
def onClick(handler: () => unit): unit = {
    // Register the handler
}

onClick(() => println("Button clicked!"))
```

### Strategy Pattern

```panther
def processData(data: Data, strategy: (Data) => Result): Result = {
    strategy(data)
}

val fastStrategy = (data: Data) => quickProcess(data)
val thoroughStrategy = (data: Data) => deepProcess(data)

val result1 = processData(data, fastStrategy)
val result2 = processData(data, thoroughStrategy)
```
