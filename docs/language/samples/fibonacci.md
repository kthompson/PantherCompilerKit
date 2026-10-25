---
title: Fibonacci
description: Recursion compared with iteration
---

# Fibonacci

Computes the first twelve Fibonacci numbers twice, once with the obvious recursion and once with a loop, so the two columns can be seen to agree.

```panther
def fib(n: int): int =
  if (n < 2) n
  else fib(n - 1) + fib(n - 2)

def fibIterative(n: int): int = {
  var a = 0
  var b = 1
  var i = 0
  while (i < n) {
    val next = a + b
    a = b
    b = next
    i = i + 1
  }
  a
}

var i = 0
while (i <= 11) {
  println(string(i) + ": " + string(fib(i)) + " " + string(fibIterative(i)))
  i = i + 1
}
```

```output
0: 0 0
1: 1 1
2: 1 1
3: 2 2
4: 3 3
5: 5 5
6: 8 8
7: 13 13
8: 21 21
9: 34 34
10: 55 55
11: 89 89
```
