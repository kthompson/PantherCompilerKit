import panther._
import TestFramework._

object Assert {

  def some[T](option: Option[T]): T = option match {
    case Option.Some(value) => value
    case Option.None        => failed("expected Some, found None")
  }

  def none[T](option: Option[T]): unit = {
    if (option.isDefined())
      failed("expected None, found Some")
  }

  def index[T](i: int, list: List[T]): T = {
    if (i < 0 || i >= list.length)
      failed("index out of bounds: " + i)
    list.getUnsafe(i)
  }

  def empty[T](list: List[T]): unit = {
    if (!list.isEmpty)
      failed("expected empty list, found " + list.length + " items")
  }

  def assert(condition: bool, message: string): unit = {
    if (!condition) failed(message) else ()
  }

  def equal[A, B](expected: A, actual: B): unit =
    assert(expected == actual, "expected " + expected + ", got: " + actual)

  def stringEqual(expected: string, actual: string): unit =
    assert(expected == actual, "expected " + expected + ", got: " + actual)

  def intEqual(expected: int, actual: int): unit =
    assert(expected == actual, "expected " + expected + ", got: " + actual)

  def boolEqual(expected: bool, actual: bool): unit =
    assert(expected == actual, "expected " + expected + ", got: " + actual)

  def isFalse(actual: bool): unit = boolEqual(false, actual)

  def isTrue(actual: bool): unit = boolEqual(true, actual)

  def single[T](items: List[T]): T = {
    items match {
      case List.Nil => failed("expected one item, found zero")
      case List.Cons(head, tail) =>
        if (tail.isEmpty) head
        else failed("expected one item, found " + items.length)
    }
  }

}
