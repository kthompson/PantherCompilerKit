import panther._

object DictionaryModule {
  def empty[K, V]() = new Dictionary[K, V](List.Nil)
}

/** TODO: this dictionary does not handle duplicate keys
  *
  * @param list
  * @tparam K
  * @tparam V
  */
case class Dictionary[K, V](list: List[KeyValue[K, V]]) {
  val length: int = list.length

  def put(key: K, value: V): Dictionary[K, V] = {
    new Dictionary(List.Cons(KeyValue(key, value), list))
  }

  def get(key: K): Option[V] = _get(key, list)
  
  def _get(key: K, list: List[KeyValue[K, V]]): Option[V] = {
    list match {
      case List.Nil => None
      case List.Cons(KeyValue(k, v), tail) =>
        if (k == key) Some(v)
        else _get(key, tail)
    }
  }

  def contains(key: K): bool = {
    list match {
      case List.Nil => false
      case List.Cons(KeyValue(k, _), tail) =>
        if (k == key) true
        else Dictionary(tail).contains(key)
    }
  }

  def keys(): List[K] = _keys(List.Nil, list)

  def _keys(acc: List[K], pairs: List[KeyValue[K, V]]): List[K] = {
    pairs match {
      case List.Nil                        => acc
      case List.Cons(KeyValue(k, _), tail) => _keys(List.Cons(k, acc), tail)
    }
  }

  def values(): List[V] = _values(List.Nil, list)

  def _values(acc: List[V], pairs: List[KeyValue[K, V]]): List[V] = {
    pairs match {
      case List.Nil                        => acc
      case List.Cons(KeyValue(_, v), tail) => _values(List.Cons(v, acc), tail)
    }
  }
}

case class KeyValue[K, V](key: K, value: V)