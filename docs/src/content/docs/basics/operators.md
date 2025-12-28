---
title: Operators
description: Arithmetic, comparison, and logical operations in Panther
---

Operators allow you to perform calculations, comparisons, and logical operations.

## Arithmetic Operators

### Basic Math

```panther
val sum = 10 + 5        // Addition: 15
val difference = 10 - 5 // Subtraction: 5
val product = 10 * 5    // Multiplication: 50
val quotient = 10 / 5   // Division: 2
val remainder = 10 % 3  // Modulo: 1
```

### Unary Operators

```panther
val positive = +5       // Unary plus: 5
val negative = -5       // Unary minus: -5
```

### Precedence

Operators follow standard mathematical precedence:

```panther
val result = 2 + 3 * 4  // 14 (multiplication first)
val result2 = (2 + 3) * 4  // 20 (parentheses first)
```

## Comparison Operators

Compare values and get boolean results:

```panther
val isEqual = 10 == 10          // Equal: true
val isNotEqual = 10 != 5        // Not equal: true
val isGreater = 10 > 5          // Greater than: true
val isLess = 5 < 10             // Less than: true
val isGreaterOrEqual = 10 >= 10 // Greater or equal: true
val isLessOrEqual = 5 <= 10     // Less or equal: true
```

## Logical Operators

Combine boolean values:

```panther
// AND - both must be true
val andResult = true && false   // false

// OR - at least one must be true
val orResult = true || false    // true

// NOT - inverts the boolean
val notResult = !true           // false
```

### Short-Circuit Evaluation

Logical operators use short-circuit evaluation:

```panther
// If first condition is false, second is not evaluated
val result = false && expensiveOperation()

// If first condition is true, second is not evaluated
val result2 = true || expensiveOperation()
```

## String Operators

### Concatenation

```panther
val greeting = "Hello, " + "World!"  // "Hello, World!"
val message = "Count: " + 42         // "Count: 42"
```

## Assignment Operators

### Simple Assignment

```panther
var x = 10
x = 20  // Assigns 20 to x
```

### Compound Assignment

```panther
var count = 10

count += 5   // count = count + 5  (15)
count -= 3   // count = count - 3  (12)
count *= 2   // count = count * 2  (24)
count /= 4   // count = count / 4  (6)
count %= 4   // count = count % 4  (2)
```

## Operator Precedence

From highest to lowest:

1. Unary operators: `+`, `-`, `!`
2. Multiplicative: `*`, `/`, `%`
3. Additive: `+`, `-`
4. Comparison: `<`, `<=`, `>`, `>=`
5. Equality: `==`, `!=`
6. Logical AND: `&&`
7. Logical OR: `||`

Use parentheses to override precedence:

```panther
val result1 = 2 + 3 * 4      // 14
val result2 = (2 + 3) * 4    // 20

val bool1 = true || false && false   // true
val bool2 = (true || false) && false // false
```

## Type-Specific Operators

### Numeric Types

```panther
val intResult = 10 / 3       // Integer division: 3
val floatResult = 10.0 / 3.0 // Float division: 3.333...
```

### Boolean Logic

```panther
val a = true
val b = false

val and = a && b  // false
val or = a || b   // true
val not = !a      // false

// Complex expressions
val complex = (a || b) && !b  // true
```


