* [x] Add type parameters to AST
* [x] Add generic Types (`Type` `GenericFunction` and `GenericClass`)
* [ ] Add Generic Symbols
   1. Kind of done but no instantiation
   2. instantiate by specifying TypeArguments in a new symbol with a mangled name
      1. We can use the mangled name to cache instantiated generics in the symbol tree
   3. I think all child symbols would get replicated



When to instantiate generics:
* Functions:
  1. When a generic function is called with explicit type arguments
     1. ie `identity<number>(5)`
  2. When we need to infer types for a generic function call
     1. ie `identity(5)`
* Classes:
  1. Type declaration/annotation:
     1. ie `val box: Box<string> = ...`
  2. When a generic class is instantiated with explicit type arguments
     1. ie `new Box<string>("hello")`
