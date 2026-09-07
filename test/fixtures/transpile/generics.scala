// Generic parameters, variance annotations and context bounds all survive the
// walk unchanged.
case class Box[T](value: T)

enum Tree[+T] {
  case Leaf
  case Node(left: Tree[T], value: T, right: Tree[T])
}

def firstOrElse[T](items: Array[T], fallback: T): T =
  if (items.length > 0) items(0) else fallback
