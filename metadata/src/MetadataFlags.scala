import panther._

enum MetadataFlags {
  case None
  case Static
}

object MetadataFlagsHelpers {
  def fromInt(value: int): MetadataFlags = value match {
    case 0 => MetadataFlags.None
    case 1 => MetadataFlags.Static
    case _ => panic("Invalid MetadataFlags value")
  }

  def toInt(value: MetadataFlags): int = value match {
    case MetadataFlags.None   => 0
    case MetadataFlags.Static => 1
  }
}
