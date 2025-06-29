import panther._

enum Value {
  case Int(value: int)
  case String(value: string)
  case Ref(token: TypeDefToken, value: int)
  case Unit

}
