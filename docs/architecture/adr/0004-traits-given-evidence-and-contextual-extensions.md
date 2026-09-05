# ADR 0004: Traits, given evidence, and contextual extensions

**Status:** Proposed
**Date:** 2026-09-04
**Primitives:** `parser`, `binder`, `lowering-emit` (see
[`primitives.yaml`](../primitives.yaml))
**Roadmap:** [§1.3](../../../ROADMAP.md#13-burn-down-the-diagnostics),
[§2](../../../ROADMAP.md#2-generics)

## Context

Panther needs a mechanism for expressing capabilities and relationships between
types without requiring those capabilities to be defined directly on the types
themselves.

The mechanism should support typeclass-style programming while keeping evidence
implicit and source syntax predictable.

Panther will distinguish between:

1. **Intrinsic members** — members defined directly by a type.
2. **Traits** — declarations of capabilities and associated behavior.
3. **Given implementations** — implementations of traits for particular types.
4. **Context bounds** — requirements that appropriate given evidence exists.
5. **Contextual extensions** — methods made available to values through
   applicable trait evidence.
6. **Associated members** — values and other members accessed explicitly through
   a type, such as `T.empty`.

A key design goal is that typeclass resolution should not silently introduce
arbitrary names or operators into scope.

The samples below are written in Panther's current syntax — `def`, braces, and
the lower-case primitive names — but none of them parse today. `trait`, `given`,
`operator` and the `derive` attribute are all new: none is a keyword in
`SyntaxFacts.getKeywordKind`, and the parser has no production for any of them.

## Decision

Panther will use:

- `trait` to declare capabilities.
- `given` to provide implementations/evidence.
- Context bounds such as `T: Eq` to require evidence.
- Extension methods supplied by evidence to extend the member set of applicable
  values.
- Explicit type qualification such as `T.empty` for associated values.

Evidence is compiler-managed and does not appear as an explicit parameter in
normal source code.

### Traits

A trait describes a capability or relationship involving a type.

```panther
trait Eq[T] {
  def equals(a: T, b: T): bool
}

trait Monoid[T] {
  val empty: T
  def combine(a: T, b: T): T
}
```

A trait may contain:

- Methods
- Properties
- Values
- Associated types
- Other type-level declarations

Traits do not necessarily modify the declaration of the type they describe.

### Given implementations

A `given` provides evidence that a type satisfies a trait.

```panther
given Eq[int] {
  def equals(a: int, b: int): bool = a == b
}

given Monoid[string] {
  val empty: string = ""
  def combine(a: string, b: string): string = a + b
}
```

The declaration `given Monoid[string]` establishes evidence for `Monoid[string]`.
Conceptually, this evidence is a proof that `string` satisfies the `Monoid`
proposition.

### Conditional givens

Evidence for a generic type is only meaningful relative to evidence for its
arguments: `Ord[List[T]]` cannot be written without `Ord[T]`.
[ADR 0005](0005-evidence-representation.md) already assumes these exist — its
recursive evidence is `Eq[List[Symbol]]` holding a reference to `Eq[Symbol]`.

A conditional given binds its type parameters with context bounds and separates
them from the trait head with `=>`:

```panther
given [T: Ord] => Ord[List[T]] {
  def compare(a: List[T], b: List[T]): int = ...
}
```

An unconditional given is the same form with nothing to bind, which is why
`given Eq[int] { … }` needs no separator.

`=>` is the arrow of the implication the declaration states —
`Ord[T] → Ord[List[T]]` — which is the reading the evidence-and-proofs section
below gives it. It is also where the two languages that have settled this
question landed: Haskell writes `instance Ord a => Ord [a]`, and Scala 3.6's
revised given syntax is `given [T: Ord] => Ord[List[T]]`. The token already
exists as `EqualsGreaterThanToken` and the parser uses it in only two places,
neither at declaration position.

The alternatives, and why not:

| Form | Rejected because |
| ---- | ---------------- |
| `given [T: Ord] then Ord[List[T]] { … }` | reads just as well, but spends a keyword on a separator's job and matches no precedent. Its real advantage is that `=>` will get busier once lambdas land — a bracketed type-parameter list cannot collide with a parenthesised value one, so this was judged visual load rather than ambiguity |
| `given [T: Ord] Ord[List[T]] { … }` | costs nothing and says nothing; two bracketed groups in a row do not mark which is the premise |
| `given [T] Ord[List[T]] where T: Ord { … }` | `where` scales to constraints a bound cannot express — one on a nested position, say — and is worth adding *alongside* context bounds if that need appears. It is not worth having two spellings for `T: Trait` alone |
| `given Ord[List[T]] where T: Ord { … }` | inferring parameters from free names is the only option that is not a rearrangement: `given Ord[List[Elem]]` would silently declare a parameter rather than report an unknown type |
| `given listOrd[T: Ord]: Ord[List[T]] { … }` | a name has no role under global coherence, where there is one given per pair |

Derivation produces conditional givens whether or not anyone writes one.
`[derive(Ord)]` on `enum List[out T]` can only mean "`Ord[List[T]]` when
`Ord[T]`", so every generic type that opts in yields one. **The syntax is for
what a user writes by hand, not for what the compiler's own sources need** — the
stdlib generics can all get theirs from the attribute.

**A conditional given is a template, not a singleton.**
ADR 0005 makes evidence a static singleton initialised in `$runtimeInit`; a
conditional given has no single value, because `Ord[List[int]]` and
`Ord[List[Symbol]]` are different records. Resolution instantiates the template
once per concrete need and emits a singleton for each, which works because the
required set is known whole-program. Overlap between a template and a concrete
given — `Ord[List[T]]` against `Ord[List[int]]` — is the unification case the
coherence rule already covers.

### Derived givens

Writing every given by hand does not scale. The transpiled sources declare 133
case classes and enums, and most of them are compared or printed somewhere, so
hand-written evidence would mean a `given Eq[…]` and a `given Show[…]` for
nearly all of them.

Derivation is therefore supported, and it is **explicit** — a type opts in with a
`derive` attribute:

```panther
[derive(Eq, Ord, Show)]
class Point(x: int, y: int)

[derive(Eq)]
enum Color {
  case Red
  case Green
}
```

The attribute declares a given for each named trait, owned by the type's own
declaration. Nothing is derived without the attribute: a class with no `derive`
has no `Eq[…]` evidence, so `a == b` on it is a diagnostic rather than a silent
identity comparison.

**What derivation produces**, over the **constructor parameters** in declaration
order:

| Trait  | Derived                                                              |
| ------ | -------------------------------------------------------------------- |
| `Eq`   | reference identity first, then parameter-by-parameter `Eq`            |
| `Ord`  | lexicographic over the parameters, in order                           |
| `Show` | `"Name(" + show(p1) + ", " + … + ")"`                                 |

For an enum the cases must match first, then their parameters.

Constructor parameters only. A `var` declared in the class body does not
participate — which is not a detail: `Symbol._children` is such a field, it
points back at parents, and a derived `Eq` that walked it would not terminate.

**Derivation requires evidence for every parameter type** and reports a
diagnostic naming the parameter that lacks it. Resolution keeps a stack of
in-progress goals and treats a goal already on the stack as satisfied, because
the derived member is recursive and calls itself. Without that rule `Eq[Symbol]`
does not resolve: it needs `Eq[Option[Symbol]]`, which needs `Eq[Symbol]`.

**Reference identity is part of the derived shape, not an optimisation.**
`Symbol` holds a `TextLocation`, which holds a `SourceFile(content, fileName)` —
the entire text of the file. A purely structural derived `Eq[Symbol]` compares
two whole source files on every dictionary lookup. Scala only escapes this
because every symbol in a file shares one `SourceFile` instance and the JVM
short-circuits string equality on reference identity. Derived `Eq` opens with the
same test, which is exactly what
[ADR 0003](0003-equality-on-reference-types.md)'s `Ceq`-on-references already
emits.

**A hand-written member wins.** Where a type defines the trait's member itself —
ten files define `toString` today, including `Type`, `List`, `TextLocation` and
`SourceFile` — that definition is used and nothing is derived for it. This is
load-bearing rather than a courtesy: `SourceFile.toString()` returns
`"SourceFile(" + fileName + ")"`, where a derived `Show` would print the whole
file.

**The transpiler supplies the attribute.** Scala's `case class` generates
structural `equals` and `toString`; a plain `class` gets neither. Panther has one
kind of class and cannot express that split in the declaration — but it does not
need to, because the attribute carries it. `case class` transpiles to
`[derive(Eq, Show)]`, plain `class` transpiles to no attribute, and the seven
plain classes in the sources — `AstPrinter`, `BoundAssemblyPrinter`,
`ChainEnumerator`, `ConversionClassifier`, `ExpressionLowerer`, `Heap`,
`LoweredAssemblyPrinter`, all stateful services nothing compares — correctly end
up with no equality at all.

### Scope and coherence of givens

Coherence matters more here than it would in most languages. The compiler's
symbol tables are ten distinct `Dictionary[Symbol, _]` instantiations. If two of
them resolved different `Eq[Symbol]` evidence, they would disagree about which
keys are equal, and nothing would report it.

**Panther enforces global coherence and implements lexical scope.**

The two are separable, and separating them is the point:

- **The rule** is that at most one given may exist for a `(trait, type)` pair in
  the whole program. A second one is a diagnostic wherever it is declared.
- **The mechanism** is that a given is an ordinary symbol declared in a scope,
  found by the same walk every other name uses.

The rule is the restrictive choice and the mechanism is the permissive one.
Relaxing later — to true lexical scoping, where two givens may coexist as long as
they are not both visible at one use site — means dropping the uniqueness pass,
not rebuilding resolution.

**Registration is global even though declaration is lexical.** Under strict
lexical rules a given declared in a sibling namespace is invisible, but under
global coherence it is the only candidate and should be found. So a given is
registered where every scope walk reaches it, and its declaration site is
recorded but not yet used to limit visibility. Restricting visibility is exactly
what the later relaxation turns on.

This is also the practical argument for the lexical mechanism: givens are in
scope during binding, so resolution reuses the existing symbol lookup rather than
introducing a parallel table that has to be kept in step with it. Givens get a
`SymbolKind` of their own, so metadata emission and `getMethodParameterMap` can
tell them apart — the lesson from `SymbolKind.This` in
[ADR 0002](0002-binding-this.md).

Three further rules:

- **Overlap is an error, and overlap means unification, not equality.**
  `given Eq[List[T]]` and `given Eq[List[int]]` are two givens for one pair as
  soon as `T` can be `int`. Comparing type arguments structurally would let that
  pair through. There is no specificity contest.
- **Derived givens are owned by the type's declaration.** The `derive` attribute
  places the given at the type, which satisfies the ownership half of coherence
  for free.
- **Builtin givens for `int`, `string`, `bool` and `char` are owned by the
  prelude**, alongside the existing builtins in `Binder`, and are therefore found
  by any scope walk.

**Deferred: the orphan rule.** Global coherence is usually paired with a
restriction that a given may only be declared where its trait or its type is
declared, which is what makes coherence checkable one compilation unit at a time.
Panther is whole-program today — there is no `.pnb` reader (§3.1) — so the
uniqueness pass sees every given and the orphan rule buys nothing yet. It becomes
necessary at the same moment separate compilation does, because two separately
compiled libraries could each define `given Eq[Foo]` and only collide at link
time.

**Deferred: named exceptions.** A second ordering for one type — a descending
`Ord[int]` — has no expression under this rule. The eventual escape hatch is
named givens that are not candidates for implicit resolution and must be passed
explicitly, which needs a way to pass evidence by hand that the design otherwise
avoids. Nothing in the compiler's sources needs it.

### Context bounds

A function can require trait evidence with a context bound:

```panther
def contains[T: Eq](items: List[T], value: T): bool = ...
```

The constraint `T: Eq` means that the function requires evidence satisfying
`Eq[T]`. The evidence is implicit.

The compiler may internally elaborate:

```panther
def contains[T: Eq](items: List[T], value: T): bool
```

into something conceptually equivalent to:

```panther
def contains[T](eq: Eq[T], items: List[T], value: T): bool
```

but this is an implementation detail and is not exposed in normal Panther source
code.

### Contextual extension methods

A trait may provide methods that become extensions on values of the constrained
type. For example:

```panther
trait Eq[T] {
  def equals(a: T, b: T): bool
}
```

could provide an extension `a.equals(b)` when `a: T`, `b: T`, and `Eq[T]` is
available. A function can therefore write:

```panther
def same[T: Eq](a: T, b: T): bool = a.equals(b)
```

The compiler resolves `equals` using the applicable `Eq[T]` evidence.

The method does not become an intrinsic member of `T`. It is a contextual
extension.

### No implicit operator generation

Trait members do not automatically become operators based on their names.

For example:

```panther
trait Eq[T] {
  def equals(a: T, b: T): bool
}
```

does not imply that `a == b` is valid. Likewise:

```panther
trait Monoid[T] {
  def combine(a: T, b: T): T
}
```

does not imply that `a <> b` is valid.

This prevents trait declarations from unexpectedly introducing language syntax.

The important rule is: **a normal trait member name never implicitly creates an
operator.**

### Operator declarations

Operators are associated with trait members explicitly, using the `operator`
keyword in the trait declaration:

```panther
trait Eq[T] {
  operator ==(a: T, b: T): bool
  operator !=(a: T, b: T): bool
}

trait Ord[T] {
  operator <(a: T, b: T): bool
  operator <=(a: T, b: T): bool
  operator >(a: T, b: T): bool
  operator >=(a: T, b: T): bool
}

trait Show[T] {
  def show(value: T): string
}
```

`a == b` is then valid exactly when `Eq[typeof(a)]` evidence is available, and it
resolves to that evidence's `==` member. `Show` declares no operator, so
`show` stays an ordinary contextual extension reached as `value.show()`.

Three rules keep this from becoming a general operator-overloading facility:

**Only existing operator tokens may be declared.** The set is whatever the lexer
already produces and `BinaryOperatorKind` already names. A trait cannot invent
`<>`; declaring an operator that is not already a token is a diagnostic. This is
what the no-implicit-operators rule is protecting — new syntax should come from a
language change, not from a library.

**Precedence and associativity belong to the language, not the trait.** They are
already fixed in `OperatorPrecedence` and the parser. A trait says which member
implements a token; it does not say how the token parses.

**A token may be claimed by at most one trait.** `==` belongs to `Eq` and nothing
else can declare it. Without this, `a == b` would need overload resolution across
traits, and the point of the design is that a use site resolves to one piece of
evidence.

The built-in operator table keeps its rows for the value types. Evidence
resolution is the fallback when the table has no row, which is the position
[ADR 0003](0003-equality-on-reference-types.md)'s reference-equality rule
currently occupies — it is replaced by this, not extended.

### Explicit associated values

Trait-provided values are accessed explicitly through the constrained type.

Given:

```panther
trait Monoid[T] {
  val empty: T
  def combine(a: T, b: T): T
}
```

and:

```panther
given Monoid[string] {
  val empty: string = ""
  def combine(a: string, b: string): string = a + b
}
```

a generic function can write:

```panther
def emptyValue[T: Monoid](): T = T.empty
```

The `T` in `T.empty` is explicit. Panther does not infer which constrained type
the programmer intended from the expected type of `empty`. This is intentional.

### Multiple constraints

A function may constrain multiple types with the same trait:

```panther
def makePair[T: Monoid, U: Monoid](): Tuple2[T, U] =
  new Tuple2[T, U](T.empty, U.empty)
```

Panther has no tuple literal, so the pair is built with `Tuple2`, which the
runtime already declares as `case class Tuple2[A, B](_1: A, _2: B)`.

There are two independent pieces of evidence, `Monoid[T]` and `Monoid[U]`, and
the explicit type qualification identifies which evidence is being used:
`T.empty` and `U.empty`.

This avoids ambiguity and makes the source code's dependency obvious.

### Member resolution

Panther member lookup conceptually considers two sources:

```
Intrinsic members
        +
Applicable contextual extensions
        │
        ▼
Available instance members
```

For a value `value.foo()` the compiler first considers intrinsic members of the
value's type and then applicable contextual extensions.

A contextual extension is applicable when appropriate trait evidence exists. For
example:

```panther
trait Printable[T] {
  def print(value: T): string
}

given Printable[User] {
  def print(value: User): string = ...
}
```

Then:

```panther
def display[T: Printable](value: T): string = value.print()
```

is valid because `Printable[T]` is available in the context.

### Associated values vs. extensions

Panther deliberately distinguishes between `T.empty` and `value.print()`.

`T.empty` is an associated member of the trait/type relationship.
`value.print()` is a contextual extension method.

The distinction is useful because some capabilities describe properties of a
type rather than operations performed on an instance. For example, given:

```panther
trait Monoid[T] {
  val empty: T
  def combine(a: T, b: T): T
}
```

`T.empty` describes a value associated with the `Monoid[T]` relationship, while
`a.combine(b)` operates on instances.

### Evidence and proofs

Trait evidence has a natural interpretation through Curry–Howard.

A constraint `T: Monoid` can be understood as the proposition `Monoid(T)`. A
`given` provides evidence `m: Monoid[T]`, which is a proof that the proposition
holds. The compiler can use that evidence to make the trait's capabilities
available.

For example, `T: Monoid` provides access to `T.empty` and, when applicable,
`value.combine(...)`.

Thus typeclass resolution can be understood as evidence resolution / proof
search, while the actual evidence representation remains an implementation
detail.

## Rationale

### `given` instead of `instance`

The keyword `given` emphasizes that the declaration provides contextual evidence
rather than defining an intrinsic implementation relationship on the type.

It also aligns naturally with the conceptual model: `given Monoid[int]` means
"given a `Monoid[int]`, this is the evidence that satisfies it."

### Explicit `T.empty`

Requiring `T.empty` rather than `empty` has several advantages:

- Avoids ambiguous contextual values.
- Makes the source of the value obvious.
- Prevents accidental name collisions.
- Does not require expected-type inference to resolve the associated value.
- Provides a natural namespace for trait-associated members.

For example:

```panther
def foo[T: Monoid, U: Monoid](): Tuple2[T, U] =
  new Tuple2[T, U](T.empty, U.empty)
```

is immediately understandable.

### No implicit operators

Operators are part of Panther's syntax and should not appear merely because a
trait happens to contain a method with a particular name.

This prevents surprising behavior such as:

```panther
trait Foo[T] {
  def combine(a: T, b: T): T
}
```

magically introducing `a <> b`.

The relationship between a trait and an operator must be explicit.

## Consequences

### Positive

- Very little typeclass plumbing appears in user code.
- `given` clearly represents contextual evidence.
- Extension methods feel like ordinary member access.
- Associated values have an explicit and predictable syntax.
- Multiple constrained types are easy to distinguish.
- Traits cannot unexpectedly introduce arbitrary operators; an operator appears
  only where a trait declares it with `operator`.
- The design maps cleanly onto proof/evidence semantics.
- Explicit `derive` reproduces Scala's `case class` / `class` split exactly,
  which Panther's single kind of class otherwise cannot express.
- Coherence is a rule over a lexical mechanism, so relaxing it later removes a
  check rather than rewriting resolution.
- The compiler remains free in principle to represent evidence however it likes.

### Negative

- Contextual member lookup makes type checking more complex.
- Tooling must distinguish intrinsic members from contextual extensions.
- Explicit qualification is sometimes more verbose.
- Given resolution becomes part of the compiler's type-checking process.
- Global coherence means a type can have exactly one instance per trait. A second
  ordering has no expression, and Panther has no newtype to work around it.
- The uniqueness check is a whole-program pass, so it cannot run until every
  source has been seen — and it has no answer for separately compiled givens
  until the orphan rule lands.
- `derive` is opt-in, so the transpiler has to emit it for every `case class` or
  the transpiled sources silently lose equality.
- Claiming a token for one trait means `==` can never mean anything else, which
  is the intended trade but is not reversible once the sources rely on it.
- Evidence needs a runtime representation, decided in
  [ADR 0005](0005-evidence-representation.md): a record of method tokens passed
  as a hidden parameter, reached through one new opcode. `Opcode.Call` takes a
  static i4 token and there is no `Callvirt`, so calling through evidence is not
  free today — but it is two lines of VM, not a new call mechanism.

## Summary

Panther's model is:

```
trait
  │
  │ describes capability
  ▼
given
  │
  │ provides evidence
  ▼
T: Trait
  │
  ├───────────────┐
  ▼               ▼
T.member       value.extension()
associated     contextual
member         extension
```

For example:

```panther
trait Monoid[T] {
  val empty: T
  def combine(a: T, b: T): T
}

given Monoid[string] {
  val empty: string = ""
  def combine(a: string, b: string): string = a + b
}

def emptyString[T: Monoid](): T = T.empty
```

The key principles are:

- Traits describe capabilities.
- `given` provides evidence of those capabilities.
- Context bounds make evidence available implicitly.
- Evidence can provide contextual extension methods.
- Associated values are accessed explicitly through the type, e.g. `T.empty`.
- Trait member names never implicitly create operators; only an `operator`
  declaration inside a trait binds a token, and only a token the language
  already has.
- Evidence is derived only where a type asks for it with `[derive(…)]`.
- A given over a generic type binds its parameters with context bounds and states
  the implication with `=>`: `given [T: Ord] => Ord[List[T]]`.

Givens are globally coherent — one per (trait, type) — but resolved through
ordinary lexical scope, so the restriction can be lifted later without rebuilding
resolution. The representation evidence takes at runtime is decided in
[ADR 0005](0005-evidence-representation.md).
