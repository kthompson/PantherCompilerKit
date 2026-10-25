---
title: Expression Evaluator
description: Enums, pattern matching, and recursion over a tree
---

# Expression Evaluator

A compiler in miniature. It builds a small expression tree from an `enum` whose cases carry data, then walks it twice with `match`: once to print it and once to evaluate it.

```panther
enum Expr {
  case Literal(value: int)
  case Add(left: Expr, right: Expr)
  case Multiply(left: Expr, right: Expr)
  case Negate(operand: Expr)
}

def show(expr: Expr): string = {
  expr match {
    case Expr.Literal(value)        => string(value)
    case Expr.Add(left, right)      => "(" + show(left) + " + " + show(right) + ")"
    case Expr.Multiply(left, right) => "(" + show(left) + " * " + show(right) + ")"
    case Expr.Negate(operand)       => "-" + show(operand)
  }
}

def eval(expr: Expr): int = {
  expr match {
    case Expr.Literal(value)        => value
    case Expr.Add(left, right)      => eval(left) + eval(right)
    case Expr.Multiply(left, right) => eval(left) * eval(right)
    case Expr.Negate(operand)       => -eval(operand)
  }
}

def report(expr: Expr): unit =
  println(show(expr) + " = " + string(eval(expr)))

// 1 + 2
report(Expr.Add(Expr.Literal(1), Expr.Literal(2)))

// 3 * (4 + 5)
report(
  Expr.Multiply(
    Expr.Literal(3),
    Expr.Add(Expr.Literal(4), Expr.Literal(5))
  )
)

// -(6 * 7)
report(Expr.Negate(Expr.Multiply(Expr.Literal(6), Expr.Literal(7))))
```

```output
(1 + 2) = 3
(3 * (4 + 5)) = 27
-(6 * 7) = -42
```
