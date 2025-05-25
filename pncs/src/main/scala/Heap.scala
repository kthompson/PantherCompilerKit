import panther._

class Heap {

  private var heapp: int = 0
  private val heapSize: int = 1024 * 1024 // 1MB
  private val heap: Array[int] = new Array[int](heapSize)

  def allocate(size: int): int = {
    if (heapp + size > heapSize) {
      panic("Heap overflow")
    }
    val addr = heapp
    heapp += size
    addr
  }


}
