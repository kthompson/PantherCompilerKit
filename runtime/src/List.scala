import panther._

object ListModule {
  //  def length[T](list: List[T]): Int = list match {
  //    case List.Nil => 0
  //    case List.Cons(_, tail) => 1 + length(tail)
  //  }
  //
  //  def reverse[T](list: List[T]): List[T] = {
  //    _reverse(List.Nil, list)
  //  }
  //
  def _reverse[T](acc: List[T], list: List[T]): List[T] = list match {
    case List.Nil => acc
    case List.Cons(head, tail) => _reverse(List.Cons(head, acc), tail)
  }

  def fromArray[T](array: Array[T]): List[T] = {
    _fromArray(List.Nil, array, array.length)
  }

  def _fromArray[T](acc: List[T], array: Array[T], index: int): List[T] = {
    if (index == 0) {
      acc
    } else {
      _fromArray(List.Cons(array(index - 1), acc), array, index - 1)
    }
  }

  def fill[T](array: Array[T], index: int, list: List[T]): int = list match {
    case List.Nil => index
    case List.Cons(head, tail) =>
      array(index) = head
      fill(array, index + 1, tail)
  }

  def fillReverse[T](array: Array[T], index: int, list: List[T]): int = list match {
    case List.Nil => index
    case List.Cons(head, tail) =>
      array(index) = head
      fillReverse(array, index - 1, tail)
  }
  
  def toString[T](list: List[T]): string = list match {
    case List.Nil => "List()"
    case List.Cons(head, tail) => _toString(head.toString, 0, tail)
  }
  
  def _toString[T](head: string,count: int, tail: List[T]): string = {
    tail match {
      case List.Nil =>
        "List(" + head + ")"
      case List.Cons(h, t) =>
        if (count == 4) {
          "List(" + head + ", ...)"
        } else if (head == "") {
          _toString(h.toString, count + 1, t)
        } else {
          _toString(head + ", " + h.toString, count + 1, t)
        }
    }
  }
}

enum List[+T] {
  case Nil
  case Cons(head: T, tail: List[T])

  val length: int = this match {
    case List.Cons(_, tail) => 1 + tail.length
    case _ => 0
  }

  def reverse(): List[T] = ListModule._reverse(List.Nil, this)

  def isEmpty(): bool = this match {
    case List.Nil => true
    case List.Cons(_, _) => false
  }

  override def toString(): string = ListModule.toString(this)

}
