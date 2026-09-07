// Members and bodies are copied rather than rewritten, so what this pins is
// that the walk preserves them exactly - including the indentation.
object Math {
  val pi = 3

  def double(n: int): int = n * 2

  def describe(n: int): string = {
    if (n < 0) {
      "negative"
    } else {
      "non-negative"
    }
  }
}
