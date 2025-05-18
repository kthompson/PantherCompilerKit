import panther._

case class SignatureBuilder() {
  var signature = new IntList()

  def toSignature(): Signature = Signature(signature.toArray())

  def write(value: int): unit = signature.add(value)

  //  def writeMethod(hasThis: bool, paramCount: int): unit = {
  //    write(if (hasThis) 1 else 0)
  //    write(paramCount)
  //  }

  def writeUnit() = write(0)

  def writeBool(): unit = write(1)

  //  def writeByte(): unit = write(2)
  def writeChar(): unit = write(3)

  def writeInt(): unit = write(4)

  //  def writeFloat(): unit = write(5)
  def writeString(): unit = write(6)

  def writeFunction(): unit = write(7)

  def writeTypeArray(length: int) = {
    write(8)
    write(length)
  }

  def writeArray() = write(9)

  def writeOption() = write(10)

  def writeTypeConstructor() = write(11)

  def writeAny() = write(12)

}
