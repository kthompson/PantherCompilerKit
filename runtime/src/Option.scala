import panther._

enum Option[+T] {
  case None
  case Some(value: T)
  
  def isEmpty(): bool = this match {
    case Option.None => true
    case Option.Some(_) => false
  }
  
  def isDefined(): bool = !isEmpty()
  
  def get(): T = this match {
    case Option.Some(value) => value
    case Option.None => panic("Option is empty")
  }

}

object OptionModule {
  def product[A, B](a: Option[A], b: Option[B]): Option[Tuple2[A, B]] = {
    a match {
      case Option.None => Option.None
      case Option.Some(aValue) =>
        b match {
          case Option.None => Option.None
          case Option.Some(bValue) => Option.Some(Tuple2(aValue, bValue))
        }
    }
  }
}
