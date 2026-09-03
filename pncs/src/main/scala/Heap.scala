import panther._

class Heap() {

  var heapp: int = 0
  val heapSize: int = 1024 * 1024 // 1MB
  val heap: Array[int] = new Array[int](heapSize)

  def allocate(size: int): int = {
    if (heapp + size > heapSize) {
      panic("Heap overflow")
    }
    val addr = heapp
    heapp = heapp + size
    addr
  }

}
