import panther._



enum SymbolKind {
  case Namespace
  case Object
  case Class
  case Enum

  case TypeParameter(variance: Variance)

  // Typed symbols
  case Field
  case Method
  case Parameter
}

case class Symbol(name: string, location: TextLocation, kind: SymbolKind, parent: Option[Symbol]) {

  // used in binder
  var _id: int = -1

  var _children: Dictionary[string, Symbol] = DictionaryModule.empty()

  def members(): List[Symbol] = _children.values()

  def tryDefine(name: string, location: TextLocation, symbolType: SymbolKind): Either[TextLocation, Symbol] = {
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
        val symbol = Symbol(name, TextLocationFactory.empty(), SymbolKind.Namespace, Option.Some(this))
        _children = _children.put(name, symbol)
        symbol
    }
  }

  def tryDefineObject(name: string, location: TextLocation): Either[TextLocation, Symbol] =
    tryDefine(name, location, SymbolKind.Object)

  def defineObject(name: string, location: TextLocation): Symbol =
    tryDefineObject(name, location) match {
      case Either.Left(_) => panic("Symbol " + name + " already exists!")
      case Either.Right(symbol) => symbol
    }

  def tryDefineEnum(name: string, location: TextLocation): Either[TextLocation, Symbol] =
    tryDefine(name, location, SymbolKind.Enum)

  def defineEnum(name: string, location: TextLocation): Symbol =
    tryDefineEnum(name, location) match {
      case Either.Left(_) => panic("Symbol " + name + " already exists!")
      case Either.Right(symbol) => symbol
    }

  def tryDefineClass(name: string, location: TextLocation): Either[TextLocation, Symbol] =
    tryDefine(name, location, SymbolKind.Class)

  def defineClass(name: string, location: TextLocation): Symbol =
    tryDefineClass(name, location) match {
      case Either.Left(_) => panic("Symbol " + name + " already exists!")
      case Either.Right(symbol) => symbol
    }

  def tryDefineTypeParameter(name: string, location: TextLocation, variance: Variance): Either[TextLocation, Symbol] =
    tryDefine(name, location, SymbolKind.TypeParameter(variance))

  def defineTypeParameter(name: string, location: TextLocation, variance: Variance): Symbol =
    tryDefineTypeParameter(name, location, variance) match {
      case Either.Left(_) => panic("Symbol " + name + " already exists!")
      case Either.Right(symbol) => symbol
    }

  def tryDefineField(name: string, location: TextLocation): Either[TextLocation, Symbol] =
    tryDefine(name, location, SymbolKind.Field)

  def defineField(name: string, location: TextLocation): Symbol =
    tryDefineField(name, location) match {
      case Either.Left(_) => panic("Symbol " + name + " already exists!")
      case Either.Right(symbol) => symbol
    }

  def tryDefineMethod(name: string, location: TextLocation): Either[TextLocation, Symbol] =
    tryDefine(name, location, SymbolKind.Method)

  def defineMethod(name: string, location: TextLocation): Symbol =
    tryDefineMethod(name, location) match {
      case Either.Left(_) => panic("Symbol " + name + " already exists!")
      case Either.Right(symbol) => symbol
    }

  def tryDefineParameter(name: string, location: TextLocation): Either[TextLocation, Symbol] =
    tryDefine(name, location, SymbolKind.Parameter)

  def defineParameter(name: string, location: TextLocation): Symbol =
    tryDefineParameter(name, location) match {
      case Either.Left(_) => panic("Symbol " + name + " already exists!")
      case Either.Right(symbol) => symbol
    }

  def qualifiedName(): string = this._qualifiedName("")

  def _qualifiedName(suffix: string): string = {
    parent match {
      case Option.Some(parent) => parent._qualifiedName(
        if (suffix == "") name else name + "." + suffix
      )
      case Option.None => name + suffix
    }
  }

  def ns(): string = {
    parent match {
      case Option.None => ""
      case Option.Some(value) => value.qualifiedName()
    }
  }

  def lookup(name: string): Option[Symbol] = {
    _children.get(name) match {
      case Option.Some(value) => Some(value)
      case Option.None =>

        parent match {
          case Option.Some(p) => p.lookup(name)
          case Option.None => None
        }
    }
  }
}

case class Scope(current: Symbol, imports: List[Symbol]) {

  /**
   * create or define a namespace
   *
   * @param name
   * @return
   */
  def enter(name: string): Scope =
    enterSymbol(current.enter(name))

  def enterSymbol(symbol: Symbol): Scope = Scope(symbol, imports)

  def defineEnum(name: string, location: TextLocation): Either[TextLocation, Symbol] =
    current.tryDefineEnum(name, location)

  def defineObject(name: string, location: TextLocation): Either[TextLocation, Symbol] =
    current.tryDefineObject(name, location)

  def defineClass(name: string, location: TextLocation): Either[TextLocation, Symbol] =
    current.tryDefineClass(name, location)

  def defineTypeParameter(name: string, location: TextLocation, variance: Variance): Either[TextLocation, Symbol] =
    current.tryDefineTypeParameter(name, location, variance)

  def defineField(name: string, location: TextLocation): Either[TextLocation, Symbol] =
    current.tryDefineField(name, location)

  def defineMethod(name: string, location: TextLocation): Either[TextLocation, Symbol] =
    current.tryDefineMethod(name, location)

  def defineParameter(name: string, location: TextLocation): Either[TextLocation, Symbol] =
    current.tryDefineParameter(name, location)

  /**
   *  add the symbol to the current scope
   *
   * @param symbol
   * @return
   */
  def use(symbol: Symbol): Scope =
    Scope(current, List.Cons(symbol, imports))

  def lookup(name: string): Option[Symbol] = {
    // if our lookup in the current symbol fails, we look in the imported symbols
    current.lookup(name) match {
      case Option.Some(value) => Some(value)
      case Option.None        => _lookup(name, imports)
    }
  }

  def _lookup(name: string, imports: List[Symbol]): Option[Symbol] = {
    imports match {
      case List.Nil => None
      case List.Cons(head, tail) =>
        head.lookup(name) match {
          case Option.Some(value) => Some(value)
          case Option.None        => _lookup(name, tail)
        }
    }
  }
}
//
//case class BoundSymbol(name: string, typ: BoundSymbolType, parent: BoundScope) {
//  val _members: Option[BoundScope] = None
//}

//case class BoundScope(parent: Option[BoundScope]) {
//  var symbols: Dictionary[string, Type] = DictionaryModule.empty()
//
//  def declareVariable(name: string, t: Type): bool = {
//    if (symbols.contains(name)) {
//      false
//    } else {
//      symbols = symbols.put(name, t)
//      true
//    }
//  }
//
//  def lookupVariable(name: string): Option[ScopeSymbol] = {
//    symbols.get(name) match {
//      case Option.Some(value) => Some(ScopeSymbol(name, value))
//      case Option.None =>
//        parent match {
//          case Option.Some(p) => p.lookupVariable(name)
//          case Option.None    => None
//        }
//    }
//  }
//}