import panther._
import system.io.File

/** A compiled image: the instructions, the metadata tables, and where to start.
  *
  * The whole thing is a list of ints — every table already serializes to one —
  * written out four bytes per int, most significant first. Bytes rather than
  * text because the values are addresses, tokens and opcodes rather than
  * anything anyone reads.
  *
  * Layout:
  *
  * {{{
  *   magic       one int, "PNB\0"
  *   version     one int
  *   entry       one int, the entry method's token, or -1 for none
  *   chunk       size, then that many instructions, then that many lines
  *   metadata    typeDefs, fields, methods, params, strings, signatures
  * }}}
  */
object PnbFile {

  /** "PNB\0" as an int, so a file that is not one is refused rather than read
    * as garbage.
    *
    * Spelled in decimal because Panther has no hex literals — `0x504e4200`
    * lexes as `0` followed by an identifier. The bytes are 80, 78, 66, 0.
    */
  val magic = 1347305984

  /** Read and write have to agree, and neither is stable yet: the tables have
    * changed shape twice while this was dead code. An image from a different
    * version is refused rather than misread.
    */
  val version = 1

  /** No entry point, for an image that is a library rather than a program. */
  val noEntry = -1

  def write(
      path: string,
      chunk: Chunk,
      metadata: Metadata,
      entry: Option[MethodToken]
  ): unit = {
    val buffer = new IntList()
    buffer.add(magic)
    buffer.add(version)
    buffer.add(entry match {
      case Option.Some(token) => token.token
      case Option.None        => noEntry
    })
    chunk.write(buffer)
    metadata.write(buffer)

    File.writeAllBytes(path, toBytes(buffer))
  }

  def read(path: string): PnbImage = {
    val buffer = fromBytes(File.readAllBytes(path))

    if (buffer.size < 3) {
      panic("not a .pnb file: " + path)
    } else if (buffer.read(0) != magic) {
      panic("not a .pnb file: " + path)
    } else if (buffer.read(1) != version) {
      panic(
        "unsupported .pnb version " + string(buffer.read(1)) + " in " + path
      )
    } else {
      val entryToken = buffer.read(2)
      val entry =
        if (entryToken == noEntry) Option.None
        else Option.Some(MethodToken(entryToken))

      val chunk = new Chunk()
      val metadata = new Metadata()
      val afterChunk = chunk.read(buffer, 3)
      metadata.read(buffer, afterChunk)

      PnbImage(chunk, metadata, entry)
    }
  }

  /** Four bytes per int, most significant first, so the encoding does not
    * depend on how the host orders them.
    */
  def toBytes(buffer: IntList): Array[int] = {
    val bytes = new Array[int](buffer.size * 4)
    for (i <- 0 to (buffer.size - 1)) {
      val value = buffer.read(i)
      bytes(i * 4 + 0) = (value >> 24) & 255
      bytes(i * 4 + 1) = (value >> 16) & 255
      bytes(i * 4 + 2) = (value >> 8) & 255
      bytes(i * 4 + 3) = value & 255
    }
    bytes
  }

  /** Each byte is masked back to 0..255 before it is shifted: a byte read from
    * a file arrives signed, so 0xFF is -1 and would otherwise flood the int
    * above it with ones.
    */
  def fromBytes(bytes: Array[int]): IntList = {
    val buffer = new IntList()
    val count = bytes.length / 4
    for (i <- 0 to (count - 1)) {
      val b0 = bytes(i * 4 + 0) & 255
      val b1 = bytes(i * 4 + 1) & 255
      val b2 = bytes(i * 4 + 2) & 255
      val b3 = bytes(i * 4 + 3) & 255
      buffer.add((b0 << 24) | (b1 << 16) | (b2 << 8) | b3)
    }
    buffer
  }
}

case class PnbImage(
    chunk: Chunk,
    metadata: Metadata,
    entry: Option[MethodToken]
)
