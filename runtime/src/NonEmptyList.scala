import panther._

case class NonEmptyList[T](head: T, list: List[T]) {
  val length = 1 + list.length
  
  def last(): T = 
    if (list.isEmpty) head 
    else list.lastUnsafe()
}

object NonEmptyListModule {
  def fromList[T](list: List[T]): Option[NonEmptyList[T]] = list match {
    case List.Nil              => None
    case List.Cons(head, tail) => Some(NonEmptyList(head, tail))
  }
}
