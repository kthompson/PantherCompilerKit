import panther._

case class Symbol(
    name: string,
    location: TextLocation,
    kind: SymbolKind,
    parent: Option[Symbol]
) {

  // used in binder
  var _id: int = -1

  var _children: Dictionary[string, Symbol] = DictionaryModule.empty()

  def members(): List[Symbol] = _children.values()

  def tryDefine(
      name: string,
      location: TextLocation,
      symbolType: SymbolKind
  ): Either[TextLocation, Symbol] = {
    _children.get(name) match {
      case Option.Some(symbol) =>
        Either.Left(symbol.location)
      case Option.None =>
        val symbol = Symbol(name, location, symbolType, Option.Some(this))
        _children = _children.put(name, symbol)
        Either.Right(symbol)
    }
  }

  def enter(name: string): Symbol = {
    _children.get(name) match {
      case Option.Some(value) => value
      case Option.None =>
        val symbol = Symbol(
          name,
          TextLocationFactory.empty(),
          SymbolKind.Namespace,
          Option.Some(this)
        )
        _children = _children.put(name, symbol)
        symbol
    }
  }

  def tryDefineObject(
      name: string,
      location: TextLocation
  ): Either[TextLocation, Symbol] =
    tryDefine(name, location, SymbolKind.Object)

  def defineObject(name: string, location: TextLocation): Symbol =
    tryDefineObject(name, location) match {
      case Either.Left(_)       => panic("Symbol " + name + " already exists!")
      case Either.Right(symbol) => symbol
    }

  def tryDefineEnum(
      name: string,
      location: TextLocation
  ): Either[TextLocation, Symbol] =
    tryDefine(name, location, SymbolKind.Enum)

  def defineEnum(name: string, location: TextLocation): Symbol =
    tryDefineEnum(name, location) match {
      case Either.Left(_)       => panic("Symbol " + name + " already exists!")
      case Either.Right(symbol) => symbol
    }

  def tryDefineClass(
      name: string,
      location: TextLocation
  ): Either[TextLocation, Symbol] =
    tryDefine(name, location, SymbolKind.Class)

  def defineClass(name: string, location: TextLocation): Symbol =
    tryDefineClass(name, location) match {
      case Either.Left(_)       => panic("Symbol " + name + " already exists!")
      case Either.Right(symbol) => symbol
    }

  def tryDefineTypeParameter(
      name: string,
      location: TextLocation,
      variance: Variance
  ): Either[TextLocation, Symbol] =
    tryDefine(name, location, SymbolKind.TypeParameter(variance))

  def defineTypeParameter(
      name: string,
      location: TextLocation,
      variance: Variance
  ): Symbol =
    tryDefineTypeParameter(name, location, variance) match {
      case Either.Left(_)       => panic("Symbol " + name + " already exists!")
      case Either.Right(symbol) => symbol
    }

  def tryDefineField(
      name: string,
      location: TextLocation
  ): Either[TextLocation, Symbol] =
    tryDefine(name, location, SymbolKind.Field)

  def defineField(name: string, location: TextLocation): Symbol =
    tryDefineField(name, location) match {
      case Either.Left(_)       => panic("Symbol " + name + " already exists!")
      case Either.Right(symbol) => symbol
    }

  def tryDefineMethod(
      name: string,
      location: TextLocation
  ): Either[TextLocation, Symbol] =
    tryDefine(name, location, SymbolKind.Method)

  def defineMethod(name: string, location: TextLocation): Symbol =
    tryDefineMethod(name, location) match {
      case Either.Left(_)       => panic("Symbol " + name + " already exists!")
      case Either.Right(symbol) => symbol
    }

  def tryDefineParameter(
      name: string,
      location: TextLocation
  ): Either[TextLocation, Symbol] =
    tryDefine(name, location, SymbolKind.Parameter)

  def defineParameter(name: string, location: TextLocation): Symbol =
    tryDefineParameter(name, location) match {
      case Either.Left(_)       => panic("Symbol " + name + " already exists!")
      case Either.Right(symbol) => symbol
    }

  def qualifiedName(): string = this._qualifiedName("")

  def _qualifiedName(suffix: string): string = {
    parent match {
      case Option.Some(parent) =>
        parent._qualifiedName(
          if (suffix == "") name else name + "." + suffix
        )
      case Option.None => name + suffix
    }
  }

  def ns(): string = {
    parent match {
      case Option.None        => ""
      case Option.Some(value) => value.qualifiedName()
    }
  }

  def lookup(name: string): Option[Symbol] = {
    _children.get(name) match {
      case Option.Some(value) => Some(value)
      case Option.None =>

        parent match {
          case Option.Some(p) => p.lookup(name)
          case Option.None    => None
        }
    }
  }
}
