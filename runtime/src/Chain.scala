import panther._

enum Chain[+T] {
  case Empty
  case Singleton(value: T)
  case Append(left: Chain[T], right: Chain[T])
  case Wrap(value: List[T])

  val isEmpty: bool = this match {
    case Chain.Empty => true
    case _           => false
  }

  def toList(): List[T] = this match {
    case Chain.Empty            => List.Nil
    case Chain.Singleton(value) => List.Cons(value, List.Nil)
    case Chain.Append(left, right) =>
      ListModule.concat(left.toList(), right.toList())
    case Chain.Wrap(value) => value
  }
}

object ChainModule {
  def concat[A](left: Chain[A], right: Chain[A]): Chain[A] =
    if (left.isEmpty) right
    else if (right.isEmpty) left
    else Chain.Append(left, right)

  def fromList[A](list: List[A]): Chain[A] = list.length match {
    case 0 => Chain.Empty
    case 1 => Chain.Singleton(list.headUnsafe())
    case _ => Chain.Wrap(list)
  }
}
