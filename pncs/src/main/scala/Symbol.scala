import panther._

case class Symbol(
    name: string,
    location: TextLocation,
    kind: SymbolKind,
    parent: Option[Symbol]
) {

  // used in binder
  var _id: int = -1
  var _blockId: int = -1
  var extern: bool = false

  var _children: Dictionary[string, Symbol] = DictionaryModule.empty()

  def isStatic(): bool = {
    parent match {
      case Option.None => false
      case Option.Some(p) =>
        p.kind == SymbolKind.Object
    }
  }

  def members(): List[Symbol] = {
    _children.values()
  }

  def fullName(): string = _fullName(name)

  def _fullName(rhs: string): string = {
    parent match {
      case Option.None => rhs
      case Option.Some(p) =>
        p._fullName(
          if (p.name == "") rhs else p.name + "." + rhs
        )
    }
  }

  def tryDefine(
      name: string,
      location: TextLocation,
      kind: SymbolKind
  ): Either[TextLocation, Symbol] = {
    _children.get(name) match {
      case Option.Some(symbol) =>
        Either.Left(symbol.location)
      case Option.None =>

        val symbol = Symbol(name, location, kind, Option.Some(this))
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

  def newBlock(): Symbol = {
    _blockId = _blockId + 1
    val name = "block$" + string(_blockId)
    val symbol = Symbol(
      name,
      TextLocationFactory.empty(),
      SymbolKind.Block,
      Option.Some(this)
    )
    // TODO: when keys can be unique, we can use the name as the key
    _children = _children.put(name, symbol)
    symbol
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

  def tryDefineAlias(
      name: string,
      location: TextLocation
  ): Either[TextLocation, Symbol] =
    tryDefine(name, location, SymbolKind.Alias)

  def defineAlias(name: string, location: TextLocation): Symbol =
    tryDefineAlias(name, location) match {
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
    tryDefine(
      name,
      location,
      if (name == ".ctor") SymbolKind.Constructor else SymbolKind.Method
    )

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

  def tryDefineLocal(
      name: string,
      location: TextLocation
  ): Either[TextLocation, Symbol] =
    tryDefine(name, location, SymbolKind.Local)

  def defineLocal(name: string, location: TextLocation): Symbol =
    tryDefineLocal(name, location) match {
      case Either.Left(_)       => panic("Symbol " + name + " already exists!")
      case Either.Right(symbol) => symbol
    }

  def qualifiedName(): string = this._qualifiedName(name)

  def _qualifiedName(suffix: string): string = {
    parent match {
      case Option.None => suffix
      case Option.Some(parent) =>
        parent._qualifiedName(
          if (parent.name == "") suffix else parent.name + "." + suffix
        )
    }
  }

  def ns(): List[string] = _ns(List.Nil)

  def _ns(list: List[string]): List[string] = {
    parent match {
      case Option.None => list
      case Option.Some(value) =>
        val acc = if (value.name == "") list else List.Cons(value.name, list)
        value._ns(acc)
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

  def lookupMember(name: string): Option[Symbol] =
    _children.get(name)

  def findSymbol(ns: List[string], name: string): Option[Symbol] = {
    ns match {
      case List.Nil => lookupMember(name)
      case List.Cons(head, tail) =>
        lookupMember(head) match {
          case Option.Some(value) =>
            value.findSymbol(tail, name)
          case Option.None =>
            Option.None
        }
    }
  }
}
