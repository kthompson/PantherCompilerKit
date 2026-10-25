---
title: Traits and Givens
description: Describing a capability once and providing it per type
---

# Traits and Givens

A **trait** names a capability, such as "can be described as a string". A
**given** provides that capability for one type. Generic code asks for the
capability with a **context bound**, and the compiler finds the right given at
each call site, so the evidence is never passed by hand.

## Declaring a Trait

A trait has one type parameter, the type that has the capability, and declares
the methods that capability provides:

```panther
trait Describe[T] {
  def describe(value: T): string
}
```

## Providing a Given

A `given` implements the trait's methods for one type. A given has no name: a
type has at most one given for each trait.

```panther
trait Describe[T] {
  def describe(value: T): string
}

given Describe[int] {
  def describe(value: int): string =
    if (value < 0) "negative " + string(-value) else "the number " + string(value)
}

given Describe[bool] {
  def describe(value: bool): string = if (value) "yes" else "no"
}
```

Declaring a second `given Describe[int]` is a compile error.

## Context Bounds

`[T: Describe]` says that `T` can be any type with a `given Describe[T]`. Inside
the function, the trait's methods are called on a value of type `T` as if they
were its own methods:

<!-- panther-context
trait Describe[T] {
  def describe(value: T): string
}

given Describe[int] {
  def describe(value: int): string =
    if (value < 0) "negative " + string(-value) else "the number " + string(value)
}

given Describe[bool] {
  def describe(value: bool): string = if (value) "yes" else "no"
}
-->

```panther
def announce[T: Describe](label: string, value: T): unit =
  println(label + ": " + value.describe())

announce("count", 42)
announce("delta", -7)
announce("ready", true)
```

```output
count: the number 42
delta: negative 7
ready: yes
```

`value.describe()` becomes a call to `describe(value)` in the given the caller
supplied. Calling `announce` with a type that has no given is a compile error at
the call site:

<!-- panther-check: expect-error -->
```panther
def announce[T: Describe](label: string, value: T): unit =
  println(label + ": " + value.describe())

announce("name", "Panther")  // error: No given instance for Describe<string>
```

A type parameter can have several bounds, and a function can bound several type
parameters: `def both[A: Describe, B: Describe](a: A, b: B)`. Classes take
context bounds too: `class Labelled[T: Describe](value: T)`.

The method-call form also works on a concrete type that has a given:

```panther
val n = 42
println(n.describe())
```

```output
the number 42
```

## Built-in Traits

The compiler provides three traits, with givens for the primitive types:

| Trait | Members | Givens for |
| --- | --- | --- |
| `Eq[T]` | `operator ==`, `operator !=` | `int`, `string`, `bool`, `char` |
| `Ord[T]` | `operator <`, `<=`, `>`, `>=` | `int`, `string`, `char` |
| `Show[T]` | `def show(value: T): string` | `int`, `string`, `bool`, `char` |

`Eq` and `Ord` declare operators, so a bounded type parameter can be compared
with the usual operators:

```panther
def largest[T: Ord](a: T, b: T): T = if (a > b) a else b

def render[T: Show](value: T): string = "<" + value.show() + ">"

println(render(largest(3, 9)))
println(render(largest("pear", "apple")))
```

```output
<9>
<pear>
```

These names are taken: a trait of your own cannot be called `Eq`, `Ord` or
`Show`.

## Operators

A trait member can be an operator instead of a method, declared with
`operator`. Giving `Eq` for your own class changes what `==` means on it. Without
a given, `==` on two objects compares identity, so two separately constructed
boxes with the same contents are unequal:

```panther
class Box(value: int)

given Eq[Box] {
  operator ==(a: Box, b: Box): bool = a.value == b.value
  operator !=(a: Box, b: Box): bool = a.value != b.value
}

println(string(new Box(4) == new Box(4)))
println(string(new Box(4) != new Box(5)))
```

```output
true
true
```

Only operator tokens the language already has can be declared, and each token
belongs to one trait: `==` and `!=` to `Eq`, and the comparisons to `Ord`.

## Deriving

Writing `Eq`, `Ord` or `Show` by hand for every class is repetitive.
`[derive(...)]` before a class or an enum generates the givens instead:

```panther
[derive(Eq, Ord, Show)]
class Point(x: int, y: int)

def render[T: Show](value: T): string = value.show()

val a = new Point(1, 2)
val b = new Point(1, 3)

println(render(a))
println(string(a == new Point(1, 2)))
println(string(a < b))
```

```output
Point(1, 2)
true
true
```

The derived givens work field by field, in declaration order:

- `Eq` is true when every field is equal.
- `Ord` compares the first field, then the next one on a tie.
- `Show` prints the class name and its fields, like `Point(1, 2)`.

Every field's type needs the same given, so a class with a field of your own
type needs that type derived too. Enums derive the same way. Two values of the
same case compare field by field, values of different cases are unequal, and
`Ord` orders cases by the order they are declared in:

```panther
[derive(Eq, Ord, Show)]
enum Shape {
  case Circle(radius: int)
  case Square(side: int)
}

def render[T: Show](value: T): string = value.show()

println(render(Shape.Circle(7)))
println(string(Shape.Circle(7) == Shape.Circle(7)))
println(string(Shape.Circle(9) < Shape.Square(1)))
```

```output
Circle(7)
true
true
```

Only `Eq`, `Ord` and `Show` can be derived.

## Conditional Givens

A given for a generic type can depend on a given for its type argument. This
one says "a `Box[T]` can be described whenever a `T` can", using the
`Describe` trait from above:

```panther
class Box[T](value: T)

given [T: Describe] => Describe[Box[T]] {
  def describe(box: Box[T]): string = "a box holding " + box.value.describe()
}

def announce[T: Describe](value: T): unit = println(value.describe())

announce(new Box[int](3))
announce(new Box[Box[int]](new Box[int](3)))
```

```output
a box holding the number 3
a box holding a box holding the number 3
```

The premise, `T: Describe`, is available inside the given's body, which is what
lets it call `describe` on the box's contents.

## Not Supported Yet

- **Associated members.** A trait cannot declare a `val`, and `T.empty`-style
  access to a member of the type itself does not resolve.
- **Literal receivers.** `42.describe()` crashes the compiler; bind the
  literal to a `val` first
  ([#83](https://github.com/kthompson/PantherCompilerKit/issues/83)).

The design and its implementation history are in
[ADR 0004](../../architecture/adr/0004-traits-given-evidence-and-contextual-extensions)
and [ADR 0006](../../architecture/adr/0006-conditional-givens).
