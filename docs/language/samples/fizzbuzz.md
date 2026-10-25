---
title: FizzBuzz
description: Loops, conditions, and building strings
---

# FizzBuzz

Prints the numbers 1 to 15, replacing multiples of three with "fizz", multiples of five with "buzz", and multiples of both with "fizzbuzz". It uses a `while` loop, an `if`/`else if` chain, and the built-in `mod`.

```panther
var i = 1
while (i <= 15) {
  val byThree = mod(i, 3) == 0
  val byFive = mod(i, 5) == 0

  if (byThree && byFive) {
    println("fizzbuzz")
  } else if (byThree) {
    println("fizz")
  } else if (byFive) {
    println("buzz")
  } else {
    println(string(i))
  }

  i = i + 1
}
```

```output
1
2
fizz
4
buzz
fizz
7
8
fizz
buzz
11
fizz
13
14
fizzbuzz
```
