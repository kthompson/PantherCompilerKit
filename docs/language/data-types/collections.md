---
title: Collections
description: Arrays, lists, and grouping values
---

# Collections

Panther has fixed-size arrays built in, and the standard library provides an immutable `List`.

## Arrays

Arrays are fixed-size collections created with `new Array[T](size)`. There are no array literals, so elements are assigned individually. Indexing uses parentheses, not brackets:

```panther
// Array initialization
val numbers = new Array[int](5)
numbers(0) = 1
numbers(1) = 2
numbers(2) = 3
numbers(3) = 4
numbers(4) = 5

// Access elements
val first = numbers(0)
val second = numbers(1)

// Modify elements
numbers(0) = 10

// Array length
val size = numbers.length
```

## Lists

The compiler automatically includes the standard library's `List[T]` type. It is a classic recursive "cons list": either `Nil` (empty) or `Cons(head, tail)`:

```panther
val numbers = List.Cons(1, List.Cons(2, List.Cons(3, List.Nil)))

val first = numbers match {
  case List.Cons(head, _) => head
  case List.Nil => 0
}

println(string(first))
```

## Grouping Values

There is no built-in tuple syntax (no `(int, string)` type and no `(1, "a")` literal), but a small class does the same job:

```panther
class Pair[A, B](first: A, second: B)

val pair = new Pair[int, string](42, "answer")
val first = pair.first    // 42
val second = pair.second  // "answer"
```
