---
title: Custom Types
description: Classes, generic types, and enums
---

# Custom Types

Define your own types with `class` and `enum`.

## Classes

Define custom data types using classes. Constructor parameters are automatically accessible as fields:

```panther
class Person(name: string, age: int) {
  def greet(): string = "Hello, my name is " + name
}

val person = new Person("Alice", 30)
println(person.greet())
```

## Generic Types

Create reusable types with type parameters, written in square brackets:

```panther
class Box[T](value: T) {
  def get(): T = value
}

val intBox = new Box[int](42)
val stringBox = new Box[string]("hello")

println(string(intBox.get()))
println(stringBox.get())
```

## Enums

Enumerations define a type with a fixed set of values. Each case goes on its own line, and matches must use the qualified name:

```panther
enum Color {
  case Red
  case Green
  case Blue
}

val color: Color = Color.Red

val name = color match {
  case Color.Red => "Red"
  case Color.Green => "Green"
  case Color.Blue => "Blue"
}

println(name)
```

## Enums With Data (Discriminated Unions)

There is no separate `union` keyword — an enum case can carry its own fields, which is how Panther expresses discriminated unions:

```panther
enum Shape {
  case Circle(radius: int)
  case Rectangle(width: int, height: int)
  case Triangle(base: int, height: int)
}

def area(shape: Shape): int = shape match {
  case Shape.Circle(r) => r * r * 3
  case Shape.Rectangle(w, h) => w * h
  case Shape.Triangle(b, h) => b * h / 2
}

val circle = Shape.Circle(5)
val circleArea = area(circle)
println(string(circleArea))
```

Cases with and without fields can be mixed in one enum. Construct a case by
calling it with its fields in declaration order, and take it apart with
`match`. Each pattern binds the case's fields by position:

```panther
enum Animal {
  case Dog(age: int)
  case Cat
  case Bird(canFly: bool)
}

def describe(animal: Animal): string = animal match {
  case Animal.Dog(age) => "a dog, age " + string(age)
  case Animal.Cat => "a cat"
  case Animal.Bird(canFly) => {
    if (canFly) "a bird that flies" else "a bird that walks"
  }
}

println(describe(Animal.Dog(5)))
println(describe(Animal.Cat))
println(describe(Animal.Bird(true)))
```

A `match` on an enum should cover every case; see
[Pattern Matching](../flow-control/pattern-matching#exhaustiveness).
