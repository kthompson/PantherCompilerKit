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
    new Dictionary[K, V](List.Cons(KeyValue(key, value), _remove(key, list)))
  }

  def getUnsafe(key: K): V = {
    get(key) match {
      case Option.Some(value) => value
      case Option.None        => panic("Key not found")
    }
  }

  def get(key: K): Option[V] = _get(key, list)

  /** Iterative for the reason `_remove` is: depth is the size of the
    * dictionary, and this is the hottest method on it.
    */
  def _get(key: K, input: List[KeyValue[K, V]]): Option[V] = {
    var rest = input
    var found: Option[V] = Option.None

    while (found.isEmpty() && !rest.isEmpty) {
      rest match {
        case List.Nil => rest = List.Nil
        case List.Cons(KeyValue(k, v), tail) =>
          if (k == key) found = Option.Some(v) else ()
          rest = tail
      }
    }

    found
  }

  def remove(key: K): Dictionary[K, V] =
    new Dictionary[K, V](_remove(key, list))

  /** Iterative on purpose.
    *
    * The natural shape — `List.Cons(kv, _remove(key, tail))` — recurses inside
    * a constructor argument, so its depth is the size of the dictionary, and
    * `put` calls it on every insert. An accumulator version is no better here:
    * these methods are not final, so neither Scala nor the VM turns the tail
    * call into a jump. The symbol table gets large enough while compiling the
    * compiler's own sources to overflow the stack either way.
    *
    * Every walk over `list` in this class is written this way, for the same
    * reason.
    */
  def _remove(key: K, list: List[KeyValue[K, V]]): List[KeyValue[K, V]] = {
    var acc: List[KeyValue[K, V]] = List.Nil
    var rest = list
    var removed = false

    while (!rest.isEmpty) {
      rest match {
        case List.Nil            => rest = List.Nil
        case List.Cons(kv, next) =>
          // guarded so only the first match goes, matching the recursive
          // version this replaced
          if (!removed && kv.key == key) {
            removed = true
          } else {
            acc = List.Cons(kv, acc)
          }
          rest = next
      }
    }

    acc.reverse()
  }

  def contains(key: K): bool = !get(key).isEmpty()

  def keys(): List[K] = _keys(List.Nil, list)

  def _keys(acc: List[K], pairs: List[KeyValue[K, V]]): List[K] = {
    var result = acc
    var rest = pairs

    while (!rest.isEmpty) {
      rest match {
        case List.Nil => rest = List.Nil
        case List.Cons(KeyValue(k, _), tail) =>
          result = List.Cons(k, result)
          rest = tail
      }
    }

    result
  }

  def values(): List[V] = _values(List.Nil, list)

  def _values(acc: List[V], pairs: List[KeyValue[K, V]]): List[V] = {
    var result = acc
    var rest = pairs

    while (!rest.isEmpty) {
      rest match {
        case List.Nil => rest = List.Nil
        case List.Cons(KeyValue(_, v), tail) =>
          result = List.Cons(v, result)
          rest = tail
      }
    }

    result
  }
}

case class KeyValue[K, V](key: K, value: V)
