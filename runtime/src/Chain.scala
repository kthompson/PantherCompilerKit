import panther._

enum Chain[+T] {
  case Empty
  case Singleton(value: T)
  case Append(left: Chain[T], right: Chain[T])
  case Wrap(value: List[T])

  def isEmpty(): bool = this match {
    case Chain.Empty       => true
    case Chain.Wrap(value) => value.isEmpty
    case _                 => false
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
    if (left.isEmpty()) right
    else if (right.isEmpty()) left
    else Chain.Append(left, right)

  def fromList[A](list: List[A]): Chain[A] = list.length match {
    case 0 => Chain.Empty
    case 1 => Chain.Singleton(list.headUnsafe())
    case _ => Chain.Wrap(list)
  }

  def uncons[A](chain: Chain[A]): Option[Tuple2[A, Chain[A]]] = chain match {
    case Chain.Empty            => Option.None
    case Chain.Singleton(value) => Option.Some(Tuple2(value, Chain.Empty))
    case Chain.Append(left, right) =>
      uncons(left) match {
        case Option.Some(Tuple2(head, tail)) =>
          Option.Some(Tuple2(head, ChainModule.concat(tail, right)))
        case Option.None => uncons(right)
      }
    case Chain.Wrap(value) =>
      value match {
        case List.Nil => Option.None
        case List.Cons(head, tail) =>
          Option.Some(Tuple2(head, Chain.Wrap(tail)))
      }
  }
}

class ChainEnumerator[A](chain: Chain[A]) {
  var moveFirst = true
  var _current: Chain[A] = chain

  def moveNext(): bool = {
    ChainModule.uncons(_current) match {
      case Option.Some(Tuple2(_, tail)) =>
        if (moveFirst) {
          moveFirst = false
        } else {
          _current = tail
        }
        !_current.isEmpty()
      case Option.None => false
    }
  }

  def current(): A = ChainModule.uncons(_current) match {
    case Option.Some(Tuple2(head, _)) => head
    case Option.None                  => panic("No current element")
  }

  def reset(): unit = {
    _current = chain
    moveFirst = true
  }
}
