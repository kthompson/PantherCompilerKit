---
title: Comments
description: Documenting your Panther code
---

Comments help document your code for yourself and other developers.

## Single-Line Comments

Use `//` for single-line comments:

```panther
// This is a single-line comment
val x = 42  // Comment after code
```

## Multi-Line Comments

Use `/* */` for comments spanning multiple lines:

<!-- panther-check: parse-only -->
```panther
/* This is a multi-line comment
   that spans several lines
   and provides detailed documentation */
   
val result = calculateValue()
```

## Block Comments Do Not Nest

Unlike some languages, Panther block comments do **not** nest. The first `*/` closes the comment, even if it looks like it should close an inner one:

```panther
/* This block comment
   ends at the first closing marker */
val x = 1
```

Avoid writing a `/* */` comment inside another `/* */` comment — the outer comment will end early, and whatever follows will be parsed as code.

## Documentation Comments

While Panther doesn't have special doc comments yet, use a consistent style:

```panther
// calculateArea computes the area of a rectangle
// Parameters:
//   width - the width of the rectangle
//   height - the height of the rectangle
// Returns: the calculated area
def calculateArea(width: int, height: int): int = {
    width * height
}
```

## Best Practices

### Do Comment

**Why, not what:**
<!-- panther-check: parse-only -->
```panther
// Use binary search because the list is sorted
val index = binarySearch(sortedList, target)
```

**Complex algorithms:**
<!-- panther-check: parse-only -->
```panther
// Implements Dijkstra's shortest path algorithm
def findShortestPath(graph: Graph, start: Node, end: Node): Path = {
    // Implementation...
}
```

**Non-obvious decisions:**
```panther
// Multiply by 1000 to convert kilometers to meters
val kilometers = 5
val meters = kilometers * 1000
```

### Don't Comment

**Obvious code:**
```panther
var counter = 0

// Bad: comment just repeats the code
// Increment counter by 1
counter = counter + 1

// Good: self-documenting code
counter = counter + 1
```

**Outdated information:**
```panther
val value = 10

// Bad: comment doesn't match code
// Divide by 2
val result = value * 3  // Comment is wrong!
```

## Commenting Out Code

Use comments to temporarily disable code during development:

```panther
// val debugMode = true
val debugMode = false

/* Temporarily disabled for testing
def experimentalFeature() {
    // ...
}
*/
```

**Note:** Remove commented-out code before committing to version control.

## TODO Comments

Mark areas that need work:

<!-- panther-check: parse-only -->
```panther
// TODO: Add error handling
def processFile(filename: string): string = {
    readFile(filename)
}
```

```panther
// FIXME: This crashes with negative values
def calculateSquareRoot(n: int): int = {
    var result = 0
    while (result * result < n) {
        result = result + 1
    }
    result
}
```

## Header Comments

Consider adding headers to files:

```panther
/*
 * Module: StringUtils
 * Purpose: Utility functions for string manipulation
 * Author: Your Name
 * Date: 2025-12-27
 */

def reverseString(s: string): string = {
    // Implementation...
    s
}
```


