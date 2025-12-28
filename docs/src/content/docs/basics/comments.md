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

```panther
/* This is a multi-line comment
   that spans several lines
   and provides detailed documentation */
   
val result = calculateValue()
```

## Nested Comments

Multi-line comments can be nested:

```panther
/* Outer comment
   /* Inner comment */
   More outer comment */
```

## Documentation Comments

While Panther doesn't have special doc comments yet, use a consistent style:

```panther
// calculateArea computes the area of a rectangle
// Parameters:
//   width - the width of the rectangle
//   height - the height of the rectangle
// Returns: the calculated area
def calculateArea(width: float, height: float): float = {
    width * height
}
```

## Best Practices

### Do Comment

**Why, not what:**
```panther
// Use binary search because the list is sorted
val index = binarySearch(sortedList, target)
```

**Complex algorithms:**
```panther
// Implements Dijkstra's shortest path algorithm
def findShortestPath(graph: Graph, start: Node, end: Node): Path {
    // Implementation...
}
```

**Non-obvious decisions:**
```panther
// Multiply by 0.621371 to convert kilometers to miles
val miles = kilometers * 0.621371
```

### Don't Comment

**Obvious code:**
```panther
// Bad: comment just repeats the code
// Increment counter by 1
counter = counter + 1

// Good: self-documenting code
counter = counter + 1
```

**Outdated information:**
```panther
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

```panther
// TODO: Add error handling
def processFile(filename: string): Data {
    return readFile(filename)
}

// FIXME: This crashes with negative values
def calculateSquareRoot(n: float): float {
    return Math.sqrt(n)
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

def reverseString(s: string): string {
    // Implementation...
}
```


