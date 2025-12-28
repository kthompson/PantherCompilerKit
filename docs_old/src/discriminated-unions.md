# Discriminated Unions

Discriminated unions, also known as tagged unions or variant types, are a powerful feature in Panther that allows you to define a type that can hold one of several different, but fixed, types. This is particularly useful for representing data that can take on multiple forms, each with potentially different data.

#### Syntax

The syntax for defining a discriminated union in Panther is as follows:

```
enum <UnionName> {
    case <Case1>(<field1>: <type1>, <field2>: <type2>, ...)
    case <Case2>(<field1>: <type1>, ...)
    ...
}
```

Each case within the union can have a different set of fields and types. Here is an example of a discriminated union representing different types of animals:

```
enum Animal {
    case Dog(age: int)
    case Cat()
    case Bird(feathers: bool)
}
```

In this example, `Animal` is a discriminated union with three cases:

- `Dog` which has a single field `age` of type `int`.
- `Cat` which has no fields.
- `Bird` which has a single field `feathers` of type `bool`.

#### Usage

To create an instance of a discriminated union, you specify the case and provide the necessary fields:

```
// Creating instances of the Animal union
val myDog = Dog(age: 5)
val myCat = Cat()
val myBird = Bird(feathers: true)
```

#### Pattern Matching

One of the key features of discriminated unions is the ability to use pattern matching to destructure and work with the different cases. Here is an example of how you might use pattern matching with the `Animal` union:

```
myAnimal match {
    case Dog(age) => {
        // Handle the Dog case
        println("This is a dog, age " + age)
    }
    case Cat() => {
        // Handle the Cat case
        println("This is a cat")
    }
    case Bird(feathers) => {
        // Handle the Bird case
        println("This is a bird with feathers: " + feathers)
    }
}
```

In this example, the `match` statement is used to determine which case of the `Animal` union is being dealt with, and then execute the appropriate code for each case.

#### Benefits

- **Type Safety**: Discriminated unions provide compile-time type checking, ensuring that all possible cases are handled.
- **Clarity**: They make the code more readable and maintainable by clearly defining the different forms a type can take.
- **Flexibility**: They allow for complex data structures to be represented in a concise and expressive way.

Discriminated unions are a versatile and powerful feature that can greatly enhance the expressiveness and safety of your code. Use them to represent data that can take on multiple forms and leverage pattern matching to handle each form appropriately.
