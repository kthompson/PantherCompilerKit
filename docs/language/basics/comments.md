---
title: Comments
description: Documenting your Panther code
---

# Comments

Comments are ignored by the compiler. Panther has line comments and block comments.

## Single-Line Comments

Use `//` for single-line comments:

```panther
// This is a single-line comment
val x = 42  // Comment after code
```

## Multi-Line Comments

Use `/* */` for comments spanning multiple lines:

```panther
/* This is a multi-line comment
   that spans several lines */
val result = 42
```

## Block Comments Do Not Nest

Unlike some languages, Panther block comments do **not** nest. The first `*/` closes the comment, even if it looks like it should close an inner one:

So a `/* */` comment inside another one ends the outer comment early, and the
rest is parsed as code:

<!-- panther-check: expect-error -->
```panther
/* outer /* inner */ this is no longer a comment */  // error: Unexpected token SlashToken, expected expression
val x = 1
```

## Documentation Comments

Panther has no separate documentation-comment syntax. A comment above a
declaration is an ordinary `//` or `/* */` comment.
