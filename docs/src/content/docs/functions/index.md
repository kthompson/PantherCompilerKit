---
title: Functions
description: Learn how to define and use functions in Panther
---

Functions are the building blocks of Panther programs. They allow you to organize code into reusable units.

## Topics

Learn about functions in Panther:

- **[Defining Functions](defining-functions)** - Basic function syntax and structure
- **[Parameters](parameters)** - Working with function parameters
- **[Return Values](return-values)** - Returning values from functions
- **[Higher-Order Functions](higher-order-functions)** - Functions as first-class values

## Quick Example

```panther
def greet(name: string): string = {
    "Hello, " + name + "!"
}

val message = greet("World")
println(message)
```
