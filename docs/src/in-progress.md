# In Progress

* complete transpile job
* rewrite the checker to work based off of all the symbols and then get their declarations and convert into a
  bound/typed tree

# Ideas

`with` expression for defining a scope that is only valid for the expression
idk.... it looks a little gross but it could be useful for some things
example usage:

```scala
with x = 5 {
  x + 1
}


if (x == 12) 5
else
with v = x * x {
  v + 1
}
```

equivelent to:

```scala
{
  val x = 5
  x + 1
}

if (x == 12) 5
else {
  val v = x * x
  v + 1
}
```