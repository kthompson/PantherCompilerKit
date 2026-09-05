import panther._

case class Members(
    objects: List[Namespaced[MemberSyntax.ObjectDeclarationSyntax]],
    classes: List[Namespaced[MemberSyntax.ClassDeclarationSyntax]],
    functions: List[MemberSyntax.FunctionDeclarationSyntax],
    enums: List[Namespaced[MemberSyntax.EnumDeclarationSyntax]],
    traits: List[Namespaced[MemberSyntax.TraitDeclarationSyntax]],
    givens: List[Namespaced[MemberSyntax.GivenDeclarationSyntax]],
    fields: List[MemberSyntax.VariableDeclaration],

    // top level variable declarations are converted to top level assignments so that we can maintain the order
    // within the set of statements. the statements are then moved to the relevant constructor
    globalStatements: List[MemberSyntax.GlobalStatementSyntax]
)

case class Namespaced[A](ns: List[string], value: A)

enum BindingMember {
  case Method(value: MemberSyntax.FunctionDeclarationSyntax)
  case Field(value: MemberSyntax.VariableDeclaration)
  case Parameter(value: ParameterSyntax)
}

enum TypingMember {
  case Method(
      genericParameters: List[GenericTypeParameter],
      // context bounds, as trait types applied to the parameter they constrain
      constraints: List[Type],
      parameters: List[BoundParameter],
      returnType: Option[Type],
      expression: Option[Expression],
      scope: Scope
  )
  case Field(options: FieldOptions, scope: Scope)
}

enum FieldOptions {
  case TypeAndExpression(fieldType: Type, expression: Expression)
  case TypeOnly(fieldType: Type)
  case ExpressionOnly(expression: Expression)
}

/** A resolved proof: which given was used, with what type arguments, and the
  * evidence its own constraints needed.
  *
  * The tree is what ADR 0005's recursive evidence describes —
  * `Eq[List[Symbol]]` holding a reference to `Eq[Symbol]`.
  */
case class Evidence(
    symbol: Symbol,
    head: Type,
    typeArguments: List[Type],
    dependencies: List[Evidence]
)

/** A `given` as the resolver needs it: what it proves, and what it needs first.
  *
  * `head` is the trait applied to its arguments — `Eq[int]`, or `Ord[List[$0]]`
  * for a conditional given, where `$0` is the given's own type parameter.
  * `constraints` are that given's context bounds, the premises of the
  * implication.
  */
case class BoundGiven(
    symbol: Symbol,
    head: Type,
    generics: List[GenericTypeParameter],
    constraints: List[Type],
    location: TextLocation
)

/** A `derive` attribute that has been registered and is waiting for a body.
  *
  * The two halves happen in different passes because they need different
  * things: registering needs only the type, so that coherence sees the given
  * and evidence records can be laid out, while a body reads the constructor
  * parameters' field types, which are not bound until later
  * ([ADR 0004](../../../docs/architecture/adr/0004-traits-given-evidence-and-contextual-extensions.md)).
  */
case class Derivation(
    typeSymbol: Symbol,
    typ: Type,
    traitSymbol: Symbol,
    givenSymbol: Symbol,
    parameterNames: List[string],
    location: TextLocation
)

case class ConstructorParams(
    genericTypeParameters: List[GenericTypeParameter],
    constraints: List[Type],
    parameters: List[ParameterSyntax]
)

case class Binder(
    trees: List[SyntaxTree],
    rootSymbol: Symbol,
    diagnosticBag: DiagnosticBag
) {

  var symbolTypes: Dictionary[int, SymbolLinks] = DictionaryModule.empty()
  var nextSymbolId = 0

  def setSymbolType(symbol: Symbol, typ: Type): Type = {
    val id = getSymbolId(symbol)
    symbolTypes = symbolTypes.put(id, SymbolLinks(typ))
    typ
  }

  def getSymbolId(symbol: Symbol): int = {
    if (symbol._id == -1) {
      symbol._id = nextSymbolId
      nextSymbolId = nextSymbolId + 1
    }
    symbol._id
  }

  def tryGetSymbolType(symbol: Symbol): Option[Type] =
    symbolTypes.get(symbol._id) match {
      case Option.None        => Option.None
      case Option.Some(value) => Option.Some(value.typ)
    }

  def getTypeSymbol(typ: Type): Option[Symbol] = {
    typ match {
      case Type.Class(_, _, _, _, symbol)        => Option.Some(symbol)
      case Type.GenericClass(_, _, _, _, symbol) => Option.Some(symbol)
      case Type.Alias(_, _, _, _, _, symbol)     => Option.Some(symbol)

      case Type.GenericFunction(_, _, _, _, symbol) => Option.None
      case Type.Function(_, _, symbol)              => Option.None
      case Type.Union(_, _)                         => Option.None
      case Type.Variable(_, _)                      => Option.None
      case Type.Any                                 => Option.None
      case Type.Never                               => Option.None
      case Type.Error(_)                            => Option.None
    }
  }

  val noLoc: TextLocation = TextLocationFactory.empty()

  val pantherNamespace =
    rootSymbol // TODO: move these to the panther namespace .enter("panther")

  // create symbols
  val anySymbol = pantherNamespace.defineClass("any", noLoc)
  val intSymbol = pantherNamespace.defineClass("int", noLoc)
  val stringSymbol = pantherNamespace.defineClass("string", noLoc)
  val boolSymbol = pantherNamespace.defineClass("bool", noLoc)
  val charSymbol = pantherNamespace.defineClass("char", noLoc)
  val unitSymbol = pantherNamespace.defineClass("unit", noLoc)

  // create types
  val anyType = Type.Any
  setSymbolType(anySymbol, anyType)

  val neverType = Type.Never

  val stringType = classType("string", stringSymbol)
  val intType = classType("int", intSymbol)
  val charType = classType("char", charSymbol)
  val boolType = classType("bool", boolSymbol)
  val unitType = classType("unit", unitSymbol)
//  val noneType =
//    new Type.Class(
//      noLoc,
//      List.Nil,
//      "Option",
//      List.Cons(Type.Never, List.Nil),
//      Option.None
//
//    )

  // assign types to symbols
  setSymbolType(stringSymbol, stringType)
  setSymbolType(intSymbol, intType)
  setSymbolType(boolSymbol, boolType)
  setSymbolType(charSymbol, charType)
  setSymbolType(unitSymbol, unitType)

  val arraySymbol = pantherNamespace.defineClass(
    "Array",
    noLoc
  )
  val arrayTSymbol =
    arraySymbol.defineTypeParameter("T", noLoc, Variance.Invariant)

  val T: GenericTypeParameter =
    GenericTypeParameter(noLoc, "T", Variance.Invariant, Option.None)

  val arrayCtorSymbol = arraySymbol.defineMethod(".ctor", noLoc)
  arrayCtorSymbol.extern = true
  val arraySizeSymbol: Symbol = param(arrayCtorSymbol, "size", intType)

  setSymbolType(
    arrayCtorSymbol,
    Type.Function(
      noLoc,
      List.Cons(
        BoundParameter(
          arraySizeSymbol,
          intType
        ),
        List.Nil
      ),
      unitType
    )
  )

  setSymbolType(
    arraySymbol,
    Type.GenericClass(
      noLoc,
      List.Nil,
      "Array",
      ListModule.one(T),
      arraySymbol
    )
  )

  // array.length: int
  val arrayLength: Symbol = arraySymbol.defineField(
    "length",
    noLoc,
    true
  )
  arrayLength.extern = true
  setSymbolType(arrayLength, intType)

  val arrayApply: Symbol = arraySymbol.defineMethod("apply", noLoc)
  val arrayApplyIndex: Symbol = arrayApply.defineParameter("index", noLoc)
  setSymbolType(arrayApplyIndex, intType)
  setSymbolType(
    arrayApply,
    Type.Function(
      noLoc,
      List.Cons(
        BoundParameter(arrayApplyIndex, intType),
        List.Nil
      ),
      Type.Variable(noLoc, 0)
    )
  )

  //  string.length: int
  setSymbolType(
    stringSymbol.defineField(
      "length",
      noLoc,
      true
    ),
    intType
  )

  // TODO: move these to the panther namespace .enter("predef")
  val predef = pantherNamespace
//    .defineObject(
//    "predef",
//    TextLocationFactory.empty()
//  )

  // println(message: string): unit
  val printlnSymbol = method(predef, "println")
  val printlnMessageSymbol = param(printlnSymbol, "message", stringType)

  setSymbolType(
    printlnSymbol,
    Type.Function(
      noLoc,
      List.Cons(
        BoundParameter(printlnMessageSymbol, anyType),
        List.Nil
      ),
      unitType
    )
  )

  // print(message: string): unit
  val printSymbol = method(predef, "print")
  val printMessageSymbol: Symbol = param(printSymbol, "message", stringType)

  setSymbolType(
    printSymbol,
    Type.Function(
      noLoc,
      List.Cons(
        BoundParameter(printMessageSymbol, anyType),
        List.Nil
      ),
      unitType
    )
  )

  // string(value: any): string - type conversion function
  conversionMethod(stringSymbol, stringType)

  // int(value: any): int - type conversion function
  conversionMethod(intSymbol, intType)

  // bool(value: any): bool - type conversion function
  conversionMethod(boolSymbol, boolType)

  // tochar(value: any): char - type conversion function
  conversionMethod(charSymbol, charType)

  // panic(message: string): never
  val panicSymbol = method(predef, "panic")
  val panicMessageSymbol = param(panicSymbol, "message", stringType)
  setSymbolType(
    panicSymbol,
    Type.Function(
      noLoc,
      List.Cons(
        BoundParameter(panicMessageSymbol, stringType),
        List.Nil
      ),
      neverType
    )
  )

  // exit(code: int): never
  val exitSymbol = method(predef, "exit")
  val exitCodeSymbol = param(exitSymbol, "code", intType)
  setSymbolType(
    exitSymbol,
    Type.Function(
      noLoc,
      List.Cons(
        BoundParameter(exitCodeSymbol, intType),
        List.Nil
      ),
      neverType
    )
  )

  // assert(condition: bool, message: string): unit
  val assertSymbol = method(predef, "assert")
  val assertConditionSymbol = param(assertSymbol, "condition", boolType)
  val assertMessageSymbol = param(assertSymbol, "message", stringType)
  setSymbolType(
    assertSymbol,
    Type.Function(
      noLoc,
      List.Cons(
        BoundParameter(assertConditionSymbol, boolType),
        List.Cons(
          BoundParameter(assertMessageSymbol, stringType),
          List.Nil
        )
      ),
      unitType
    )
  )

  // mod(a: int, b: int): int
  val modSymbol = method(predef, "mod")
  val modASymbol = param(modSymbol, "a", intType)
  val modBSymbol = param(modSymbol, "b", intType)
  setSymbolType(
    modSymbol,
    Type.Function(
      noLoc,
      List.Cons(
        BoundParameter(modASymbol, intType),
        List.Cons(
          BoundParameter(modBSymbol, intType),
          List.Nil
        )
      ),
      intType
    )
  )

  /** These are the methods and fields that need to be bound/symbolized
    */
  var membersToBind: Dictionary[Symbol, List[BindingMember]] =
    DictionaryModule.empty()

  /** statementsToBind represent the statements that occur in the body of a
    * class/object, these statements will be converted into a constructor for
    * the class/enum/object
    */
  var statementsToBind
      : Dictionary[Symbol, List[MemberSyntax.GlobalStatementSyntax]] =
    DictionaryModule.empty()

  /** constructors to build */
  var ctorsToBind: Dictionary[Symbol, ConstructorParams] =
    DictionaryModule.empty()

  /** functionBodies are the bound expressions for all methods and constructors
    */
  var functionBodies: Dictionary[Symbol, BoundExpression] =
    DictionaryModule.empty()

  /** map of member symbols to their untyped declarations */
  var membersToType: Dictionary[Symbol, TypingMember] = DictionaryModule.empty()

  /** static constructors to call on runtime initialization */
  var staticCtors: List[Symbol] = List.Nil

  /** every `given` in the program, in reverse source order. Global rather than
    * per-scope: coherence is a whole-program rule, so a given declared anywhere
    * is a candidate everywhere.
    */
  var givens: List[BoundGiven] = List.Nil

  /** numbers the anonymous given symbols, in source order */
  var givenCount: int = 0

  /** every `[derive(…)]` that registered a given, in reverse source order */
  var derivations: List[Derivation] = List.Nil

  /** The trait that claims each operator token, keyed by `SyntaxKind`.
    *
    * A token may be claimed by at most one trait, so that `a == b` resolves to
    * one piece of evidence rather than needing overload resolution across
    * traits ([ADR 0004](../../../docs/architecture/adr/0004-traits-given-evidence-and-contextual-extensions.md)).
    *
    * Keyed by token kind rather than by text: a kind has exactly one spelling,
    * while `SyntaxFacts.getBinaryOperatorText` gives `&&` for both `LogicalAnd`
    * and `BitwiseAnd`.
    */
  var operatorTraits: Dictionary[int, Symbol] = DictionaryModule.empty()

  /** The static field holding each given's evidence record, keyed by the
    * given's symbol. Defined once the program object exists, and filled in by
    * the emitter, which is the first place method tokens are known.
    */
  var evidenceFields: Dictionary[Symbol, Symbol] =
    DictionaryModule.empty[Symbol, Symbol]()

  /** `$runtimeInit`, so the emitter can recognise the method it has to append
    * the evidence-record construction to.
    */
  var runtimeInit: Option[Symbol] = Option.None

  // ── The prelude's traits ────────────────────────────────────────────────
  //
  // `Eq`, `Ord` and `Show`, with givens for the value types, owned by the
  // prelude alongside the builtins above and therefore found by any scope walk
  // ([ADR 0004](../../../docs/architecture/adr/0004-traits-given-evidence-and-contextual-extensions.md)).
  //
  // Declared here rather than in a source file because there is no source the
  // compiler always reads: a test snippet, a doc block and `pnc/src` are each
  // just the trees they are handed.
  //
  // These have to come after `functionBodies`, `givens` and `operatorTraits`
  // are declared — a class body initialises in order, and every one of the
  // three is written to below.

  /** The trait parameter `T` as it appears inside a trait's own members. */
  val variableT: Type = Type.Variable(noLoc, 0)

  val eqSymbol: Symbol = builtinTrait("Eq")
  builtinBinaryMember(eqSymbol, "==", variableT, boolType)
  builtinBinaryMember(eqSymbol, "!=", variableT, boolType)
  claimBuiltinOperator(SyntaxKind.EqualsEqualsToken, eqSymbol)
  claimBuiltinOperator(SyntaxKind.BangEqualsToken, eqSymbol)

  val ordSymbol: Symbol = builtinTrait("Ord")
  builtinBinaryMember(ordSymbol, "<", variableT, boolType)
  builtinBinaryMember(ordSymbol, "<=", variableT, boolType)
  builtinBinaryMember(ordSymbol, ">", variableT, boolType)
  builtinBinaryMember(ordSymbol, ">=", variableT, boolType)
  claimBuiltinOperator(SyntaxKind.LessThanToken, ordSymbol)
  claimBuiltinOperator(SyntaxKind.LessThanEqualsToken, ordSymbol)
  claimBuiltinOperator(SyntaxKind.GreaterThanToken, ordSymbol)
  claimBuiltinOperator(SyntaxKind.GreaterThanEqualsToken, ordSymbol)

  /** `Show` claims no token. `show` stays an ordinary contextual extension
    * reached as `value.show()` — ADR 0004 is explicit that a trait member name
    * never implicitly creates an operator.
    */
  val showSymbol: Symbol = builtinTrait("Show")
  builtinUnaryMember(showSymbol, "show", variableT, stringType)

  builtinEqGiven(intType, "int")
  builtinEqGiven(stringType, "string")
  builtinEqGiven(boolType, "bool")
  builtinEqGiven(charType, "char")

  // No `Ord[bool]`. The other three get their comparisons straight from the
  // builtin operator table, which has no rows for `<` on `bool`, so the
  // bodies would have to be spelled out — and `false < true` is not a
  // question the sources ask.
  builtinOrdGiven(intType, "int")
  builtinOrdGiven(stringType, "string")
  builtinOrdGiven(charType, "char")

  builtinShowGiven(intType, "int")
  builtinShowGiven(stringType, "string")
  builtinShowGiven(boolType, "bool")
  builtinShowGiven(charType, "char")

  val classifier = new ConversionClassifier(this)
  val exprBinder: ExprBinder =
    new ExprBinder(rootSymbol, this, classifier, diagnosticBag)

  /** A prelude trait: one invariant type parameter, no members yet. */
  def builtinTrait(name: string): Symbol = {
    val symbol = pantherNamespace.defineTrait(name, noLoc)
    symbol.defineTypeParameter("T", noLoc, Variance.Invariant)
    setSymbolType(
      symbol,
      Type.GenericClass(noLoc, List.Nil, name, ListModule.one(T), symbol)
    )
    symbol
  }

  def claimBuiltinOperator(tokenKind: int, traitSymbol: Symbol): unit = {
    operatorTraits = operatorTraits.put(tokenKind, traitSymbol)
  }

  /** `name(a: operand, b: operand): result`, with no body — a requirement. */
  def builtinBinaryMember(
      owner: Symbol,
      name: string,
      operand: Type,
      result: Type
  ): Symbol = {
    val symbol = owner.defineMethod(name, noLoc)
    val a = param(symbol, "a", operand)
    val b = param(symbol, "b", operand)
    setSymbolType(
      symbol,
      Type.Function(
        noLoc,
        List.Cons(
          BoundParameter(a, operand),
          ListModule.one(BoundParameter(b, operand))
        ),
        result
      )
    )
    symbol
  }

  /** `name(value: operand): result`, with no body. */
  def builtinUnaryMember(
      owner: Symbol,
      name: string,
      operand: Type,
      result: Type
  ): Symbol = {
    val symbol = owner.defineMethod(name, noLoc)
    val value = param(symbol, "value", operand)
    setSymbolType(
      symbol,
      Type.Function(noLoc, ListModule.one(BoundParameter(value, operand)), result)
    )
    symbol
  }

  /** Registers a prelude given and returns its symbol.
    *
    * Named after what it proves rather than numbered. `bindGiven` numbers the
    * givens it binds so that the numbering is deterministic across compilers;
    * a prelude given is already deterministic, and a name keeps the user's
    * `$given$0` the first one they wrote.
    */
  def builtinGiven(traitSymbol: Symbol, typ: Type, typeName: string): Symbol = {
    val name = "$given$" + traitSymbol.name + "$" + typeName
    val symbol = pantherNamespace.tryDefineGiven(name, noLoc) match {
      case Either.Left(_)       => panic("prelude given " + name + " exists")
      case Either.Right(symbol) => symbol
    }
    val head =
      Type.Class(
        noLoc,
        List.Nil,
        traitSymbol.name,
        ListModule.one(typ),
        traitSymbol
      )
    setSymbolType(symbol, head)
    registerGiven(BoundGiven(symbol, head, List.Nil, List.Nil, noLoc))
    symbol
  }

  /** A prelude given's member, whose body is the operator the builtin table
    * already has a row for: `Eq[int].==` is `a == b` on two ints.
    *
    * The body is a bound expression rather than parsed source, so it skips
    * type checking — which is the point, since the check it would go through
    * is the one that sends `a == b` back here looking for evidence.
    */
  def builtinBinaryBody(
      owner: Symbol,
      name: string,
      operand: Type,
      operator: BinaryOperatorKind,
      result: Type
  ): unit = {
    val symbol = builtinBinaryMember(owner, name, operand, result)
    symbol.members() match {
      case List.Cons(a, List.Cons(b, _)) =>
        functionBodies = functionBodies.put(
          symbol,
          BoundExpression.Binary(
            noLoc,
            BoundExpression.Variable(noLoc, a, Option.Some(operand)),
            operator,
            BoundExpression.Variable(noLoc, b, Option.Some(operand)),
            result
          )
        )
      case _ => panic("builtinBinaryBody: " + name + " has no parameters")
    }
  }

  def builtinEqGiven(typ: Type, typeName: string): unit = {
    val symbol = builtinGiven(eqSymbol, typ, typeName)
    builtinBinaryBody(symbol, "==", typ, BinaryOperatorKind.Equals, boolType)
    builtinBinaryBody(symbol, "!=", typ, BinaryOperatorKind.NotEquals, boolType)
  }

  def builtinOrdGiven(typ: Type, typeName: string): unit = {
    val symbol = builtinGiven(ordSymbol, typ, typeName)
    builtinBinaryBody(symbol, "<", typ, BinaryOperatorKind.LessThan, boolType)
    builtinBinaryBody(
      symbol,
      "<=",
      typ,
      BinaryOperatorKind.LessThanOrEqual,
      boolType
    )
    builtinBinaryBody(symbol, ">", typ, BinaryOperatorKind.GreaterThan, boolType)
    builtinBinaryBody(
      symbol,
      ">=",
      typ,
      BinaryOperatorKind.GreaterThanOrEqual,
      boolType
    )
  }

  /** `Show[T].show` is the conversion function the language already has.
    * `string(value)` is an extern call the emitter turns into `ConvStr`.
    */
  def builtinShowGiven(typ: Type, typeName: string): unit = {
    val symbol = builtinGiven(showSymbol, typ, typeName)
    val member = builtinUnaryMember(symbol, "show", typ, stringType)
    val conversion = stringSymbol.lookupMember("apply") match {
      case Option.Some(apply) => apply
      case Option.None => panic("builtinShowGiven: string has no conversion")
    }

    member.members() match {
      case List.Cons(value, _) =>
        functionBodies = functionBodies.put(
          member,
          BoundExpression.Call(
            noLoc,
            Option.None,
            conversion,
            List.Nil,
            ListModule.one(
              BoundExpression.Variable(noLoc, value, Option.Some(typ))
            ),
            stringType
          )
        )
      case List.Nil => panic("builtinShowGiven: show has no parameter")
    }
  }

  def conversionMethod(symbol: Symbol, returnType: Type): unit = {
    val apply = method(symbol, "apply")
    setSymbolType(
      apply,
      Type.Function(
        noLoc,
        List.Cons(
          BoundParameter(param(apply, "value", anyType), anyType),
          List.Nil
        ),
        returnType
      )
    )
  }

  def classType(name: string, symbol: Symbol): Type =
    new Type.Class(noLoc, List.Nil, name, List.Nil, symbol)

  def method(parent: Symbol, name: string): Symbol = {
    val symbol = parent.defineMethod(name, noLoc)
    symbol.extern = true
    symbol
  }

  def param(parent: Symbol, name: string, typ: Type): Symbol = {
    val symbol = parent.defineParameter(name, noLoc)
    setSymbolType(symbol, typ)
    symbol
  }

  def bind(): BoundAssembly = {
    // take ast, extract top level statements
    val members = splitMembersInTrees(trees)

    // make sure there are not more than one source file with top level statements
    detectMultipleSourceFilesWithTopLevelStatements(
      members.functions,
      members.globalStatements
    )

    // start binding/typing analysis
    val rootScope = Scope(rootSymbol, List.Nil)

    // bind all objects and classes first
    bindClassesObjectAndEnums(
      members.classes,
      members.objects,
      members.enums,
      members.traits,
      members.givens,
      rootScope
    )

    val program = getProgramSymbol(rootSymbol)
    val init = getRuntimeInit(program)
    runtimeInit = Option.Some(init)

    // The evidence records live on the program object, which makes them static
    // and so reachable from anywhere. They cannot be defined while binding the
    // givens themselves: the program object does not exist yet.
    defineEvidenceRecords(givens, program)

    // bind global statements, fields, and functions
    addMembersToBind(program, members.functions, members.fields, List.Nil)

    // bind all members function & field types with type annotations
    membersToType = bindMembers(membersToBind.list, 1, DictionaryModule.empty())

    val main = getMainMethod(program)

    addStatementsToBind(main, members.globalStatements)

    // TODO: this method still needs to register the field assignments as ctor statements
    bindConstructorSignatures(ctorsToBind.list)

    // Derived bodies read the types of the fields the constructor parameters
    // became, so they cannot be built until those fields are typed. The givens
    // themselves were registered while their types were bound, which is what
    // let coherence and the evidence-record layout see them.
    buildDerivedBodies(derivations)

    // then bind all functions & fields without type annotations
    bindTypingMembers()

    // bind all `statementsToBind` as constructor bodies
    buildConstructorBodies()

//    panic("the type above probably should include the symbols we started to type, as well as all the parameters for methods")

    // then bind all function bodies

    // build runtime initialization function body
    buildRuntimeInitBody(init, staticCtors, List.Nil)

    // inject the initialization of static constructors
    patchMainWithInit(main, init)

    BoundAssembly(
      diagnosticBag.diagnostics,
      functionBodies,
      Option.Some(main)
    )
  }

  def buildRuntimeInitBody(
      init: Symbol,
      staticCtors: List[Symbol],
      statements: List[BoundStatement]
  ): unit = {
    staticCtors match {
      case List.Nil =>
        functionBodies = functionBodies
          .put(
            init,
            BoundExpression.Block(
              statements,
              BoundExpression.Unit(noLoc)
            )
          )
      case List.Cons(head, tail) =>
        // call the static constructor
        val call = BoundStatement.ExpressionStatement(
          BoundExpression.Call(
            noLoc,
            Option.None,
            head,
            List.Nil,
            List.Nil,
            unitType
          )
        )

        // recursively build the body for the next static ctor
        buildRuntimeInitBody(init, tail, List.Cons(call, statements))
    }

  }

  def patchMainWithInit(main: Symbol, init: Symbol): unit = {
    val mainBody = functionBodies.get(main) match {
      case Option.None =>
        // main has no body, so we create a new one
        BoundExpression.Unit(noLoc)
      case Option.Some(value) =>
        // main already has a body, so we add the call to init at the start
        value
    }

    val newMainBody = BoundExpression.Block(
      List.Cons(
        BoundStatement.ExpressionStatement(
          BoundExpression.Call(
            noLoc,
            Option.None,
            init,
            List.Nil,
            List.Nil,
            unitType
          )
        ),
        List.Nil
      ),
      mainBody
    )

    functionBodies = functionBodies
      .remove(main)
      .put(main, newMainBody)
  }

  def buildConstructorBodies(): unit = {
    statementsToBind.list match {
      case List.Nil => ()
      case List.Cons(KeyValue(symbol, statements), tail) =>
        buildConstructorBody(symbol, statements)
        buildConstructorBodies()
    }
  }

  def buildConstructorBody(
      symbol: Symbol,
      globalStatements: List[MemberSyntax.GlobalStatementSyntax]
  ): unit = {
    statementsToBind = statementsToBind.remove(symbol)
    val ctor = findCtorSymbol(symbol)
    val scope = Scope(ctor, List.Nil) // todo: add imports
    val ctorBody = functionBodies.get(ctor) match {
      case Option.None =>
        val statements =
          exprBinder.bindGlobalStatements(globalStatements, scope)
        functionBodies = functionBodies.put(
          ctor,
          BoundExpression.Block(
            statements,
            BoundExpression.Unit(noLoc)
          )
        )
        // Only type a constructor that has none. `bindConstructorSignatures`
        // has already given every class constructor its parameters and
        // constraints, and overwriting that with a nullary signature loses
        // them.
        tryGetSymbolType(ctor) match {
          case Option.Some(_) => ()
          case Option.None =>
            setSymbolType(
              ctor,
              Type.Function(symbol.location, List.Nil, unitType)
            )
            ()
        }

      case Option.Some(value) =>
        val statements =
          exprBinder.bindGlobalStatements(globalStatements, scope)
        functionBodies = functionBodies.put(
          ctor,
          BoundExpression.Block(
            statements,
            value
          )
        )
    }
  }

  def findCtorSymbol(symbol: Symbol): Symbol = {
    if (symbol.kind == SymbolKind.Method) {
      symbol
    } else {
      symbol.lookupMember(".ctor") match {
        case Option.None        => symbol.defineMethod(".ctor", symbol.location)
        case Option.Some(value) => value
      }
    }
  }

  def bindConstructorSignatures(
      list: List[KeyValue[Symbol, ConstructorParams]]
  ): unit = {
    list match {
      case List.Nil => ()
      case List.Cons(KeyValue(symbol, ctorParams), tail) =>
        val scope = Scope(symbol, List.Nil)

        scope.defineMethod(".ctor", symbol.location) match {
          case Either.Left(location) =>
            diagnosticBag.reportDuplicateDefinition(
              ".ctor",
              location,
              symbol.location
            )
          case Either.Right(ctorSymbol) =>
            val ctor = scope.enterSymbol(ctorSymbol)
            val params = bindParameters(ctorParams.parameters, ctor)
            val typParams = ctorParams.genericTypeParameters

            // The constructor is elaborated like any other method; the field
            // below is what makes the evidence reachable from instance methods
            // through `this` (ADR 0005, decision B).
            defineEvidenceParameters(
              ctorParams.constraints,
              typParams,
              symbol.location,
              ctor
            )
            defineEvidenceFields(
              ctorParams.constraints,
              typParams,
              symbol.location,
              scope
            )
            val typ = if (typParams.isEmpty) {
              Type.Function(symbol.location, params, unitType)
            } else {
              // A constrained class carries its constraints on `.ctor`: the
              // constructor is elaborated like any other method, and the `new`
              // site is where the type arguments are concrete
              // (ADR 0005, decision B).
              Type.GenericFunction(
                symbol.location,
                typParams,
                ctorParams.constraints,
                params,
                unitType
              )
            }
            setSymbolType(ctorSymbol, typ)

            // Add object constructors to static initialization list
            if (symbol.kind == SymbolKind.Object) {
              staticCtors = List.Cons(ctorSymbol, staticCtors)
            }
        }

        bindConstructorSignatures(tail)
    }
  }

  def bindTypingMembers(): unit = {
    membersToType.list match {
      case List.Nil => ()
      case List.Cons(KeyValue(symbol, member), tail) =>
        bindOneTypingMember(symbol, member)
        bindTypingMembers()
    }
  }

  def bindOneTypingMember(symbol: Symbol, member: TypingMember): Type = {
    membersToType = membersToType.remove(symbol)
    member match {
      case TypingMember.Method(
            genTypeParams,
            constraints,
            parameters,
            returnType,
            expression,
            scope
          ) =>
        bindMethodBody(
          symbol,
          genTypeParams,
          constraints,
          parameters,
          returnType,
          expression,
          scope
        )
      case TypingMember.Field(options, scope) =>
        bindFieldExpression(symbol, options, scope)
    }
  }

  def bindFieldExpression(
      symbol: Symbol,
      options: FieldOptions,
      scope: Scope
  ): Type = {
    options match {
      case FieldOptions.TypeAndExpression(fieldType, expression) =>
        // TODO: this expression needs to be added to the symbol's constructor body
        val expr = exprBinder.check(expression, fieldType, scope)
        setSymbolType(symbol, fieldType)
      case FieldOptions.TypeOnly(fieldType) =>
        setSymbolType(symbol, fieldType)
      case FieldOptions.ExpressionOnly(expression) =>
        val expr = exprBinder.infer(expression, scope)
        // TODO: this expression needs to be added to the symbol's constructor body
        val returnType = getType(expr)
        setSymbolType(symbol, returnType)
    }
  }

  /** Define the receiver for an instance type.
    *
    * The symbol goes on the class or enum itself, not on each method, so that
    * method bodies, blocks, field initialisers and class-body statements all
    * reach the same symbol through the ordinary scope walk. Objects are static
    * and get none, which leaves `this` inside an object a diagnostic.
    *
    * A generic class or enum is applied to its own parameters, so `this` inside
    * `class Foo[T]` is `Foo<$0>` and not the uninstantiated `Foo`. Without
    * that, a method returning `this` cannot satisfy its own declared return
    * type of `Foo[T]`.
    */
  def defineThis(
      symbol: Symbol,
      generics: List[GenericTypeParameter]
  ): unit = {
    val typeArgs = exprBinder.genericsAsVariables(generics, 0)
    val selfType = getSymbolType(symbol) match {
      case Type.GenericClass(location, ns, name, _, classSymbol) =>
        Type.Class(location, ns, name, typeArgs, classSymbol)
      case Type.Alias(location, ns, name, _, value, aliasSymbol) =>
        Type.Alias(location, ns, name, typeArgs, value, aliasSymbol)
      case typ => typ
    }

    symbol.tryDefineThis(symbol.location) match {
      case Either.Left(_) => ()
      case Either.Right(thisSymbol) =>
        setSymbolType(thisSymbol, selfType)
    }
  }

  def bindMethodBody(
      symbol: Symbol,
      args: List[GenericTypeParameter],
      constraints: List[Type],
      parameters: List[BoundParameter],
      returnType: Option[Type],
      expression: Option[Expression],
      methodScope: Scope
  ): Type = {

    // if returnType is None, then we need to infer the type and set the methods type
    val expr = expression match {
      case Option.None => Option.None
      case Option.Some(expression) =>

        val boundExpr = returnType match {
          case Option.None => exprBinder.infer(expression, methodScope)
          case Option.Some(toType) =>
            exprBinder.check(expression, toType, methodScope)
        }

        functionBodies = functionBodies.put(symbol, boundExpr)
        Option.Some(boundExpr)
    }

    returnType match {
      case Option.None =>
        expr match {
          case Option.None =>
            // TODO: it is an error if returnType and expression are None at the moment
            panic("returnType and expression are None")

          case Option.Some(value) =>
            val ret = getType(value)
            val typ = if (args.isEmpty) {
              Type.Function(symbol.location, parameters, ret)
            } else {
              Type.GenericFunction(
                symbol.location,
                args,
                constraints,
                parameters,
                ret
              )
            }
            setSymbolType(symbol, typ)
        }
      case Option.Some(value) =>
        val typ = if (args.isEmpty) {
          Type.Function(symbol.location, parameters, value)
        } else {
          Type.GenericFunction(
            symbol.location,
            args,
            constraints,
            parameters,
            value
          )
        }
        setSymbolType(symbol, typ)
    }
  }

  def getTypes(expressions: List[BoundExpression]): List[Type] = {
    expressions match {
      case List.Nil => List.Nil
      case List.Cons(head, tail) =>
        List.Cons(getType(head), getTypes(tail))
    }
  }

  def getType(expr: BoundExpression): Type = {
    expr match {
      case BoundExpression.Error(message) => Type.Error(message)

      case _: BoundExpression.Assignment => unitType
      case _: BoundExpression.Boolean    => boolType
      case _: BoundExpression.Character  => charType
      case _: BoundExpression.For        => unitType
      case _: BoundExpression.Int        => intType
      case _: BoundExpression.Is         => boolType
      case _: BoundExpression.String     => stringType
      case _: BoundExpression.Unit       => unitType
      case _: BoundExpression.While      => unitType

      case expr: BoundExpression.ArrayCreation => expr.resultType
      case expr: BoundExpression.EvidenceCall  => expr.resultType
      case expr: BoundExpression.Binary        => expr.resultType
      case expr: BoundExpression.Block         => getType(expr.expression)
      case expr: BoundExpression.Call          => expr.resultType
      case expr: BoundExpression.Cast          => expr.targetType
      case expr: BoundExpression.Index         => expr.resultType
      case expr: BoundExpression.If            => expr.resultType
      case expr: BoundExpression.Match         => expr.resultType
      case expr: BoundExpression.MemberAccess  => expr.resultType
      case expr: BoundExpression.New           => expr.resultType
      case expr: BoundExpression.Unary         => expr.resultType
      case expr: BoundExpression.Variable =>
        expr.resultType match {
          case Option.None        => getSymbolType(expr.symbol)
          case Option.Some(value) => value
        }
    }
  }

  def getSymbolType(symbol: Symbol): Type = {
    // see if we already typed it
    tryGetSymbolType(symbol) match {
      case Option.Some(value) => value
      case Option.None        =>
        // try to determine the type based on a binding member
        membersToType.get(symbol) match {
          case Option.None =>
            // no type has been detected yet so lets bind the entry and then try again
            diagnosticBag.reportBugUnknownType(symbol.location, symbol.name)
            Type.Error("Unknown type for symbol " + symbol.kind)
          case Option.Some(value) =>
            bindOneTypingMember(symbol, value)
        }
    }
  }

  def bindMembers(
      list: List[KeyValue[Symbol, List[BindingMember]]],
      phase: int,
      acc: Dictionary[Symbol, TypingMember]
  ): Dictionary[Symbol, TypingMember] = {
    list match {
      case List.Nil                                   => acc
      case List.Cons(KeyValue(symbol, members), tail) =>
        // TODO: need to make scope that incorporates imports from the SyntaxTree
        //  that the symbol was defined in
        val scope = Scope(symbol, List.Nil)
        bindMembers(tail, phase, bindSymbolMembers(members, phase, acc, scope))
    }
  }

  /** Create symbols for all the members of a class, object, or enum
    *
    * If the member has a type annotation, set the type, otherwise add to the
    * members to type(acc)
    * @param list
    * @param phase
    * @param acc
    * @param scope
    * @return
    */
  def bindSymbolMembers(
      list: List[BindingMember],
      phase: int,
      acc: Dictionary[Symbol, TypingMember],
      scope: Scope
  ): Dictionary[Symbol, TypingMember] = {
    list match {
      case List.Nil => acc
      case List.Cons(head, tail) =>
        val newAcc = head match {
          case BindingMember.Method(value) => bindMethod(value, acc, scope)
          case BindingMember.Field(value)  => bindField(value, acc, scope)
          case BindingMember.Parameter(value) =>
            bindParameterAsField(value, acc, scope)
        }

        bindSymbolMembers(tail, phase, newAcc, scope)
    }
  }

  /** Bind a field
    *
    * @param value
    * @param scope
    * @return
    *   a TypingMember if the field was not successfully typed
    */
  def bindField(
      value: MemberSyntax.VariableDeclaration,
      acc: Dictionary[Symbol, TypingMember],
      scope: Scope
  ): Dictionary[Symbol, TypingMember] = {
    scope.defineField(
      value.identifier.text,
      value.identifier.location,
      value.valOrVarKeyword.kind == SyntaxKind.ValKeyword
    ) match {
      case Either.Left(location) =>
        diagnosticBag.reportDuplicateDefinition(
          value.identifier.text,
          location,
          value.identifier.location
        )
        // since the definition was an error there is no need to try to type it later
        acc
      case Either.Right(symbol) =>
        val options = value.typeAnnotation match {
          case Option.None =>
            // infer type later
            FieldOptions.ExpressionOnly(value.expression)
          case Option.Some(typeAnnotation) =>
            val returnType = bindTypeName(typeAnnotation.typ, scope)
            setSymbolType(symbol, returnType)
            FieldOptions.TypeAndExpression(returnType, value.expression)
        }
        acc.put(symbol, TypingMember.Field(options, scope))
    }
  }

  def bindParameterAsField(
      value: ParameterSyntax,
      acc: Dictionary[Symbol, TypingMember],
      scope: Scope
  ): Dictionary[Symbol, TypingMember] = {
    // A constructor parameter carries no val/var keyword, so nothing declares
    // it immutable. Left assignable rather than guessing.
    scope.defineField(
      value.identifier.text,
      value.identifier.location,
      false
    ) match {
      case Either.Left(location) =>
        diagnosticBag.reportDuplicateDefinition(
          value.identifier.text,
          location,
          value.identifier.location
        )
        // since the definition was an error there is no need to try to type it later
        acc
      case Either.Right(symbol) =>
        val returnType = bindTypeName(value.typeAnnotation.typ, scope)
        setSymbolType(symbol, returnType)
        acc.put(
          symbol,
          TypingMember.Field(FieldOptions.TypeOnly(returnType), scope)
        )
    }
  }

  /** Bind a method
    *
    * @param value
    *   the method to bind
    * @param scope
    * @return
    *   a TypingMember if the method was not successfully typed
    */
  def bindMethod(
      value: MemberSyntax.FunctionDeclarationSyntax,
      acc: Dictionary[Symbol, TypingMember],
      scope: Scope
  ): Dictionary[Symbol, TypingMember] = {
    val methodName = value.identifier.text
    val methodLocation = value.identifier.location
    scope.defineMethod(methodName, methodLocation) match {
      case Either.Left(location) =>
        diagnosticBag.reportDuplicateDefinition(
          methodName,
          methodLocation,
          location
        )
        // since the definition was an error there is no need to try to type it later
        acc

      case Either.Right(symbol) =>
        val methodScope = scope.enterSymbol(symbol)

        // bind generic type parameters
        val genTypeParams = value.genericParameters match {
          case Option.None => List.Nil
          case Option.Some(value) =>
            bindGenericTypeParameters(value.parameters.items, methodScope)
        }

        // Second pass over the same list: the parameters have to be defined
        // and indexed before a bound can be applied to one.
        val constraints: List[Type] = value.genericParameters match {
          case Option.None => List.Nil
          case Option.Some(value) =>
            bindGenericConstraints(value.parameters.items, methodScope)
        }

        val parameters = bindParameters(value.parameters, methodScope)

        // after the declared parameters, so they take the trailing slots
        defineEvidenceParameters(
          constraints,
          genTypeParams,
          methodLocation,
          methodScope
        )

        val expr = value.body match {
          case Option.None       => Option.None
          case Option.Some(body) => Option.Some(body.expression)
        }
        val returnType = value.typeAnnotation match {
          case Option.None => Option.None // infer type later
          case Option.Some(typeAnnotation) =>
            val returnType = bindTypeName(typeAnnotation.typ, methodScope)
            val typ = if (genTypeParams.isEmpty) {
              Type.Function(symbol.location, parameters, returnType)
            } else {
              Type.GenericFunction(
                symbol.location,
                genTypeParams,
                constraints,
                parameters,
                returnType
              )
            }
            setSymbolType(symbol, typ)
            Option.Some(returnType)
        }

        acc.put(
          symbol,
          TypingMember.Method(
            genTypeParams,
            constraints,
            parameters,
            returnType,
            expr,
            methodScope
          )
        )
    }
  }

  def bindParameters(
      parameters: List[ParameterSyntax],
      scope: Scope
  ): List[BoundParameter] = {
    parameters match {
      case List.Nil => List.Nil
      case List.Cons(head, tail) =>
        val typ = bindTypeName(head.typeAnnotation.typ, scope)
        scope.defineParameter(
          head.identifier.text,
          head.identifier.location
        ) match {
          case Either.Left(location) =>
            diagnosticBag.reportDuplicateDefinition(
              head.identifier.text,
              location,
              head.identifier.location
            )
            bindParameters(tail, scope)
          case Either.Right(symbol) =>
            setSymbolType(symbol, typ)
            List.Cons(
              BoundParameter(symbol, typ),
              bindParameters(tail, scope)
            )
        }
    }
  }

  /** Convert field declarations with initializers into assignment statements
    * that can be executed during object initialization
    */
  def fieldsToInitStatements(
      fields: List[MemberSyntax.VariableDeclaration]
  ): List[MemberSyntax.GlobalStatementSyntax] = {
    fields match {
      case List.Nil => List.Nil
      case List.Cons(field, tail) =>
        val assignmentStatement = new MemberSyntax.GlobalStatementSyntax(
          StatementSyntax.ExpressionStatement(
            Expression.Assignment(
              Expression.IdentifierName(
                SimpleNameSyntax.IdentifierNameSyntax(field.identifier)
              ),
              field.equalToken,
              field.expression
            )
          )
        )
        List.Cons(assignmentStatement, fieldsToInitStatements(tail))
    }
  }

  /** Append two lists of statements, preserving order */
  def appendStatements(
      first: List[MemberSyntax.GlobalStatementSyntax],
      second: List[MemberSyntax.GlobalStatementSyntax]
  ): List[MemberSyntax.GlobalStatementSyntax] = {
    first match {
      case List.Nil => second
      case List.Cons(head, tail) =>
        List.Cons(head, appendStatements(tail, second))
    }
  }

  def bindTypeName(name: NameSyntax, scope: Scope): Type = {
    name match {
      case NameSyntax.SimpleName(value) =>
        bindTypeSimpleName(value, true, scope)
      case NameSyntax.QualifiedName(left, _, right) =>
        val newScope = bindNameToScope(left, scope)
        bindTypeSimpleName(right, false, newScope)
    }
  }

  def bindNameToScope(name: NameSyntax, scope: Scope): Scope = {
    name match {
      case NameSyntax.SimpleName(value) =>
        bindSimpleNameToScope(value, scope)
      case NameSyntax.QualifiedName(left, _, right) =>
        val newScope = bindNameToScope(left, scope)
        bindSimpleNameToScope(right, newScope)
    }
  }

  def bindSimpleNameToScope(name: SimpleNameSyntax, scope: Scope): Scope = {
    name match {
      case SimpleNameSyntax.IdentifierNameSyntax(identifier) =>
        scope.lookup(identifier.text) match {
          case Option.None =>
            diagnosticBag.reportInvalidNamespace(identifier.location)
            scope
          case Option.Some(symbol) =>
            scope.enterSymbol(symbol)
        }
      case SimpleNameSyntax.GenericNameSyntax(identifier, typeArgumentlist) =>
        diagnosticBag.reportInvalidNamespace(identifier.location)
        scope
      case SimpleNameSyntax.ScalaAliasSyntax(open, name, arrow, alias, close) =>
        diagnosticBag.reportInvalidNamespace(
          open.location.merge(close.location)
        )
        scope
      case SimpleNameSyntax.AliasSyntax(name, asKeyword, alias) =>
        diagnosticBag.reportInvalidNamespace(
          name.location.merge(alias.location)
        )
        scope
    }
  }

  def bindTypeArgumentList(
      arguments: Array[TypeArgumentItemSyntax],
      scope: Scope
  ): List[Type] =
    bindTypeArguments(ListModule.fromArray(arguments), scope)

  def bindTypeArguments(
      arguments: List[TypeArgumentItemSyntax],
      scope: Scope
  ): List[Type] = {
    arguments match {
      case List.Nil => List.Nil
      case List.Cons(head, tail) =>
        List.Cons(
          bindTypeName(head.name, scope),
          bindTypeArguments(tail, scope)
        )
    }
  }

  def bindTypeSimpleName(
      name: SimpleNameSyntax,
      top: bool,
      scope: Scope
  ): Type = {
    name match {
      case SimpleNameSyntax.GenericNameSyntax(identifier, typeArgumentlist) =>
        scope.lookup(identifier.text) match {
          case Option.None =>
            diagnosticBag.reportTypeNotDefined(
              identifier.location,
              identifier.text
            )
            Type.Error("Type not defined: " + identifier.text)
          case Option.Some(symbol) =>
            // TODO: need to add diagnostic for verifying that there are the correct
            //  number of type arguments on `symbol`
            val typeArguments =
              bindTypeArgumentList(typeArgumentlist.arguments, scope)

            tryGetSymbolType(symbol) match {
              case Option.None =>
                // if we found the symbol but not the type then something
                // funky happened and that shouldn't be possible panic for
                // now
                panic("unimplemented: bindTypeSimpleName")
              case Option.Some(
                    Type.Class(location, ns, name, _, symbol)
                  ) =>
                new Type.Class(
                  location,
                  ns,
                  name,
                  typeArguments,
                  symbol
                )
              case Option.Some(
                    Type.Alias(location, ns, name, _, value, symbol)
                  ) =>
                new Type.Alias(
                  location,
                  ns,
                  name,
                  typeArguments,
                  value,
                  symbol
                )
              case Option.Some(
                    Type.GenericClass(location, ns, name, _, symbol)
                  ) =>
                new Type.Class(
                  location,
                  ns,
                  name,
                  typeArguments,
                  symbol
                )
              case Option.Some(value) =>
                panic(
                  "TODO: return a type with the type arguments " + symbol.location
                )

                value
            }
        }
      case SimpleNameSyntax.IdentifierNameSyntax(identifier) =>
        if (top && identifier.text == "any") {
          Type.Any
        } else if (top && identifier.text == "never") {
          Type.Never
        } else {
          scope.lookup(identifier.text) match {
            case Option.None =>
              diagnosticBag.reportTypeNotDefined(
                identifier.location,
                identifier.text
              )
              Type.Error("Type not defined: " + identifier.text)
            case Option.Some(symbol) =>
              tryGetSymbolType(symbol) match {
                case Option.None =>
                  // if we found the symbol but not the type then something
                  // funky happened and that shouldn't be possible panic for
                  // now
                  panic("unimplemented: bindTypeSimpleName")
                case Option.Some(typ) => typ
              }
          }
        }
      case SimpleNameSyntax.ScalaAliasSyntax(open, name, arrow, alias, close) =>
        diagnosticBag.reportInvalidNamespace(
          name.location.merge(close.location)
        )
        Type.Never
      case SimpleNameSyntax.AliasSyntax(name, asKeyword, alias) =>
        diagnosticBag.reportInvalidNamespace(
          name.location.merge(alias.location)
        )
        Type.Never
    }
  }

  def countMembersToBind(
      list: List[KeyValue[Symbol, List[BindingMember]]],
      fields: int,
      methods: int
  ): Tuple2[int, int] = {
    list match {
      case List.Nil => Tuple2(fields, methods)
      case List.Cons(KeyValue(symbol, members), tail) =>
        val methodsAndFields = countMembers(members, 0, 0)
        countMembersToBind(
          tail,
          fields + methodsAndFields._2,
          methods + methodsAndFields._1
        )
    }
  }

  def countMembers(
      list: List[BindingMember],
      methods: int,
      fields: int
  ): Tuple2[int, int] = {
    list match {
      case List.Nil => Tuple2(methods, fields)
      case List.Cons(BindingMember.Method(_), tail) =>
        countMembers(tail, methods + 1, fields)
      case List.Cons(BindingMember.Field(_), tail) =>
        countMembers(tail, methods, fields + 1)
      case List.Cons(BindingMember.Parameter(_), tail) =>
        countMembers(tail, methods, fields + 1)
    }
  }

  def printFunctionsToBind(
      list: List[KeyValue[Symbol, List[MemberSyntax.FunctionDeclarationSyntax]]]
  ): unit = {
    list match {
      case List.Nil => ()
      case List.Cons(head, tail) =>
        println(head.key.name)
        println("  " + string(head.value.length) + " functions")
        printFunctionsToBind(tail)
    }
  }

  def printStatementsToBind(
      list: List[KeyValue[Symbol, List[MemberSyntax.GlobalStatementSyntax]]]
  ): unit = {
    list match {
      case List.Nil => ()
      case List.Cons(head, tail) =>
        println(head.key.name)
        println("  " + string(head.value.length) + " statements")
        printStatementsToBind(tail)
    }
  }

  def bindClassesObjectAndEnums(
      classes: List[Namespaced[MemberSyntax.ClassDeclarationSyntax]],
      objects: List[Namespaced[MemberSyntax.ObjectDeclarationSyntax]],
      enums: List[Namespaced[MemberSyntax.EnumDeclarationSyntax]],
      traits: List[Namespaced[MemberSyntax.TraitDeclarationSyntax]],
      givens: List[Namespaced[MemberSyntax.GivenDeclarationSyntax]],
      scope: Scope
  ): unit = {
    // Traits first: a context bound on a class resolves its trait while the
    // class is being bound, so the trait has to already have a symbol. Traits
    // themselves name nothing at this stage — their members are deferred like
    // any other type's.
    bindTraits(traits, scope)
    bindObjects(objects, scope)
    bindClasses(classes, scope)
    bindEnums(enums, scope)
    // Givens last: a head like `Eq[Box[int]]` names both a trait and a type,
    // so everything else has to be defined first.
    bindGivens(givens, scope)
  }

  def bindTraits(
      traits: List[Namespaced[MemberSyntax.TraitDeclarationSyntax]],
      scope: Scope
  ): unit = {
    traits match {
      case List.Nil => ()
      case List.Cons(head, tail) =>
        bindTrait(head, scope)
        bindTraits(tail, scope)
    }
  }

  /** Binds a trait the way `bindClass` binds a class, minus everything to do
    * with construction: no `.ctor` is registered and no `this` is defined,
    * because a trait is never instantiated. Its members are bound so that the
    * requirements a `given` has to satisfy have symbols and types.
    */
  def bindTrait(
      head: Namespaced[MemberSyntax.TraitDeclarationSyntax],
      scope: Scope
  ): unit = {
    val name = head.value.identifier.text
    scope.defineTrait(name, head.value.identifier.location) match {
      case Either.Left(location) =>
        diagnosticBag.reportDuplicateDefinition(
          name,
          location,
          head.value.identifier.location
        )
      case Either.Right(symbol) =>
        // The annotations are load-bearing for the self-hosted compiler: it
        // does not yet widen the branches of a `match` or `if` to their common
        // supertype, so without them `args` types as
        // `List.Nil | List<GenericTypeParameter>` and `.isEmpty` is not found.
        val args: List[GenericTypeParameter] =
          head.value.genericParameters match {
            case Option.None => List.Nil
            case Option.Some(value) =>
              bindGenericTypeParameters(
                value.parameters.items,
                scope.enterSymbol(symbol)
              )
          }

        val typ: Type = if (args.isEmpty) {
          Type.Class(
            symbol.location,
            symbol.ns(),
            symbol.name,
            List.Nil,
            symbol
          )
        } else {
          Type.GenericClass(
            symbol.location,
            symbol.ns(),
            symbol.name,
            args,
            symbol
          )
        }
        setSymbolType(symbol, typ)

        val members = splitMembers(List.Nil, head.value.template.members)
        bindClassesObjectAndEnums(
          members.classes,
          members.objects,
          members.enums,
          members.traits,
          members.givens,
          scope.enterSymbol(symbol)
        )
        addMembersToBind(
          symbol,
          members.functions,
          members.fields,
          List.Nil
        )
        claimOperators(symbol, members.functions)
    }
  }

  /** Records which trait implements each operator token it declares.
    *
    * Read from the syntax rather than from the bound members because the
    * members are deferred — `addMembersToBind` queues them — and a use site
    * needs the claim before any body binds. Only a trait claims: a `given`
    * supplies the implementation for a token its trait already owns.
    */
  def claimOperators(
      traitSymbol: Symbol,
      functions: List[MemberSyntax.FunctionDeclarationSyntax]
  ): unit = {
    functions match {
      case List.Nil => ()
      case List.Cons(head, tail) =>
        if (SyntaxFacts.isBinaryOperator(head.identifier.kind)) {
          claimOperator(traitSymbol, head.identifier)
        } else ()
        claimOperators(traitSymbol, tail)
    }
  }

  def claimOperator(traitSymbol: Symbol, token: SyntaxToken): unit = {
    operatorTraits.get(token.kind) match {
      case Option.Some(owner) =>
        // The same trait declaring a token twice is a duplicate member, which
        // the symbol table reports on its own.
        if (owner != traitSymbol) {
          diagnosticBag.reportOperatorAlreadyClaimed(
            token.location,
            token.text,
            owner.name
          )
        } else ()
      case Option.None =>
        operatorTraits = operatorTraits.put(token.kind, traitSymbol)
    }
  }

  def bindEnums(
      enums: List[Namespaced[MemberSyntax.EnumDeclarationSyntax]],
      scope: Scope
  ): List[BoundDefinition] = {
    enums match {
      case List.Nil => List.Nil
      case List.Cons(head, tail) =>
        bindEnum(head, scope)
        bindEnums(tail, scope)
    }
  }

  def bindEnum(
      head: Namespaced[MemberSyntax.EnumDeclarationSyntax],
      scope: Scope
  ): unit = {
    val name = head.value.identifier.text
    val enumSymbol = scope.defineAlias(
      name,
      head.value.identifier.location
    )
    enumSymbol match {
      case Either.Left(location) =>
        diagnosticBag.reportDuplicateDefinition(
          name,
          location,
          head.value.identifier.location
        )
      case Either.Right(symbol) =>
        // bind generic type parameters
        val enumScope = scope.enterSymbol(symbol)
        val genericParameters = head.value.genericParameters match {
          case Option.None => List.Nil
          case Option.Some(value) =>
            bindGenericTypeParameters(value.parameters.items, enumScope)
        }

        val cases = ListModule.fromArray(head.value.cases)
        val caseTypes =
          bindEnumCases(cases, List.Nil, genericParameters, enumScope)

        val typ =
          Type.Alias(
            symbol.location,
            symbol.ns(),
            symbol.name,
            List.Nil,
            Type.Union(
              symbol.location,
              caseTypes
            ),
            symbol
          )
        setSymbolType(symbol, typ)
        defineThis(symbol, genericParameters)

        val members = splitMembers(List.Nil, head.value.members)
        bindClassesObjectAndEnums(
          members.classes,
          members.objects,
          members.enums,
          members.traits,
          members.givens,
          enumScope
        )

        addMembersToBind(symbol, members.functions, members.fields, List.Nil)
        addStatementsToBind(symbol, members.globalStatements)
    }
  }

  def bindEnumCases(
      cases: List[EnumCaseSyntax],
      types: List[Type],
      genericTypeParameters: List[GenericTypeParameter],
      scope: Scope
  ): List[Type] = {
    cases match {
      case List.Nil => types
      case List.Cons(enumCase, tail) =>
        val name = enumCase.identifier.text
        val location = enumCase.identifier.location
        scope.defineClass(name, location) match {
          case Either.Left(location) =>
            diagnosticBag.reportDuplicateDefinition(name, location, location)
            // return no types since we had an error
            List.Nil
          case Either.Right(caseSymbol) =>
            val parent = scope.current
            val typ =
              if (
                genericTypeParameters.isEmpty || enumCase.parameters.isEmpty()
              ) {
                new Type.Class(
                  caseSymbol.location,
                  caseSymbol.ns(),
                  caseSymbol.name,
                  List.Nil,
                  caseSymbol
                )
              } else {
                Type.GenericClass(
                  caseSymbol.location,
                  caseSymbol.ns(),
                  caseSymbol.name,
                  genericTypeParameters,
                  caseSymbol
                )
              }
            setSymbolType(caseSymbol, typ)
            enumCase.parameters match {
              case Option.None =>
              case Option.Some(value) =>
                addMembersToBind(
                  caseSymbol,
                  List.Nil,
                  List.Nil,
                  value.parameters
                )
                ctorsToBind = ctorsToBind.put(
                  caseSymbol,
                  ConstructorParams(
                    genericTypeParameters,
                    List.Nil,
                    value.parameters
                  )
                )
            }

            bindEnumCases(
              tail,
              List.Cons(typ, types),
              genericTypeParameters,
              scope
            )
        }
    }
  }

  def bindClasses(
      value: List[Namespaced[MemberSyntax.ClassDeclarationSyntax]],
      scope: Scope
  ): List[BoundDefinition] = {
    value match {
      case List.Nil => List.Nil
      case List.Cons(head, tail) =>
        bindClass(head, scope)
        bindClasses(tail, scope)
    }
  }

  def bindClass(
      head: Namespaced[MemberSyntax.ClassDeclarationSyntax],
      scope: Scope
  ): unit = {
    val name = head.value.identifier.text
    val classSymbol = scope.defineClass(
      name,
      head.value.identifier.location
    )
    classSymbol match {
      case Either.Left(location) =>
        diagnosticBag.reportDuplicateDefinition(
          name,
          location,
          head.value.identifier.location
        )
      case Either.Right(symbol) =>
        // bind generic type parameters
        head.value.genericParameters match {
          case Option.None =>
            setSymbolType(
              symbol,
              new Type.Class(
                symbol.location,
                symbol.ns(),
                symbol.name,
                List.Nil,
                symbol
              )
            )

            defineThis(symbol, List.Nil)

            ctorsToBind = ctorsToBind.put(
              symbol,
              ConstructorParams(List.Nil, List.Nil, head.value.parameters)
            )
          case Option.Some(value) =>
            val args = bindGenericTypeParameters(
              value.parameters.items,
              scope.enterSymbol(symbol)
            )
            val typ = if (args.isEmpty) {
              Type.Class(
                symbol.location,
                symbol.ns(),
                symbol.name,
                List.Nil,
                symbol
              )
            } else {
              Type.GenericClass(
                symbol.location,
                symbol.ns(),
                symbol.name,
                args,
                symbol
              )
            }
            setSymbolType(symbol, typ)
            defineThis(symbol, args)

            val constraints: List[Type] = bindGenericConstraints(
              value.parameters.items,
              scope.enterSymbol(symbol)
            )

            ctorsToBind = ctorsToBind.put(
              symbol,
              ConstructorParams(args, constraints, head.value.parameters)
            )
        }

        head.value.template match {
          case Option.None =>
            // Even classes with no template need their constructor parameters converted to accessible fields
            addMembersToBind(
              symbol,
              List.Nil,
              List.Nil,
              head.value.parameters
            )
            // ...and a constructor body, or the constructor never gets an
            // address and calling it lands on offset -1. There are no
            // statements to run, but the parameter stores the emitter adds
            // have to go somewhere.
            addStatementsToBind(symbol, List.Nil)
          case Option.Some(template) =>
            val members = splitMembers(List.Nil, template.members)
            bindClassesObjectAndEnums(
              members.classes,
              members.objects,
              members.enums,
              members.traits,
              members.givens,
              scope.enterSymbol(symbol)
            )
            addMembersToBind(
              symbol,
              members.functions,
              members.fields,
              head.value.parameters
            )
            addStatementsToBind(symbol, members.globalStatements)
        }

        registerDerivations(symbol, head.value.derives, head.value.parameters)
    }
  }

  /** Turns each trait a `derive` attribute names into a registered given with
    * declared-but-empty members. The bodies come later, in
    * `buildDerivedBodies`.
    */
  def registerDerivations(
      typeSymbol: Symbol,
      derives: Option[DeriveAttributeSyntax],
      parameters: List[ParameterSyntax]
  ): unit = {
    derives match {
      case Option.None => ()
      case Option.Some(attribute) =>
        registerDerivationList(
          typeSymbol,
          attribute.traits,
          parameterNames(parameters)
        )
    }
  }

  def registerDerivationList(
      typeSymbol: Symbol,
      traits: List[DerivedTraitSyntax],
      names: List[string]
  ): unit = {
    traits match {
      case List.Nil => ()
      case List.Cons(head, tail) =>
        registerDerivation(typeSymbol, head.name, names)
        registerDerivationList(typeSymbol, tail, names)
    }
  }

  def parameterNames(parameters: List[ParameterSyntax]): List[string] = {
    parameters match {
      case List.Nil => List.Nil
      case List.Cons(head, tail) =>
        List.Cons(head.identifier.text, parameterNames(tail))
    }
  }

  /** Only the prelude's three traits can be derived: derivation is a rule
    * about what each of them means over a list of parameters, and there is no
    * such rule for a trait the compiler has never seen.
    */
  def derivableTrait(name: string): Option[Symbol] = {
    if (name == "Eq") Option.Some(eqSymbol)
    else if (name == "Ord") Option.Some(ordSymbol)
    else if (name == "Show") Option.Some(showSymbol)
    else Option.None
  }

  def registerDerivation(
      typeSymbol: Symbol,
      name: SyntaxToken,
      names: List[string]
  ): unit = {
    derivableTrait(name.text) match {
      case Option.None =>
        diagnosticBag.reportTraitNotDerivable(name.location, name.text)
      case Option.Some(traitSymbol) =>
        tryGetSymbolType(typeSymbol) match {
          case Option.Some(Type.Class(loc, ns, typeName, List.Nil, symbol)) =>
            val typ: Type = Type.Class(loc, ns, typeName, List.Nil, symbol)
            declareDerivedGiven(typeSymbol, typ, traitSymbol, names, name)
          case _ =>
            // A generic type's derived given is conditional — `Eq[Box[T]]`
            // given `Eq[T]` — and a conditional given cannot reach its own
            // premise yet (ADR 0004).
            diagnosticBag.reportDeriveOnGenericType(
              name.location,
              typeSymbol.name
            )
        }
    }
  }

  def declareDerivedGiven(
      typeSymbol: Symbol,
      typ: Type,
      traitSymbol: Symbol,
      names: List[string],
      name: SyntaxToken
  ): unit = {
    val givenName = "$derived$" + traitSymbol.name + "$" + typeSymbol.name
    pantherNamespace.tryDefineGiven(givenName, name.location) match {
      case Either.Left(existing) =>
        diagnosticBag.reportDuplicateDefinition(
          givenName,
          existing,
          name.location
        )
      case Either.Right(givenSymbol) =>
        val head: Type =
          Type.Class(
            name.location,
            List.Nil,
            traitSymbol.name,
            ListModule.one(typ),
            traitSymbol
          )
        setSymbolType(givenSymbol, head)
        declareDerivedMembers(traitSymbol, givenSymbol, typ)
        registerGiven(
          BoundGiven(givenSymbol, head, List.Nil, List.Nil, name.location)
        )
        derivations = List.Cons(
          Derivation(
            typeSymbol,
            typ,
            traitSymbol,
            givenSymbol,
            names,
            name.location
          ),
          derivations
        )
    }
  }

  /** The members a derived given has to supply, which are the trait's own —
    * declared in the same order, because that order is the evidence record's
    * layout.
    */
  def declareDerivedMembers(
      traitSymbol: Symbol,
      givenSymbol: Symbol,
      typ: Type
  ): unit = {
    if (traitSymbol == eqSymbol) {
      builtinBinaryMember(givenSymbol, "==", typ, boolType)
      builtinBinaryMember(givenSymbol, "!=", typ, boolType)
      ()
    } else if (traitSymbol == ordSymbol) {
      builtinBinaryMember(givenSymbol, "<", typ, boolType)
      builtinBinaryMember(givenSymbol, "<=", typ, boolType)
      builtinBinaryMember(givenSymbol, ">", typ, boolType)
      builtinBinaryMember(givenSymbol, ">=", typ, boolType)
      ()
    } else {
      builtinUnaryMember(givenSymbol, "show", typ, stringType)
      ()
    }
  }

  def genericTypeParamAsType(head: GenericTypeParameter, index: int): Type =
    Type.Variable(head.location, index)

  def bindGenericTypeParameters(
      value: List[GenericParameterSyntax],
      scope: Scope
  ): List[GenericTypeParameter] = {
    bindGenericTypeParametersWithIndex(value, scope, 0)
  }

  def bindGenericTypeParametersWithIndex(
      value: List[GenericParameterSyntax],
      scope: Scope,
      index: int
  ): List[GenericTypeParameter] = {
    value match {
      case List.Nil => List.Nil
      case List.Cons(
            GenericParameterSyntax(varianceToken, identifier, _),
            tail
          ) =>
        val variance = varianceToken match {
          case Option.None => Variance.Invariant
          case Option.Some(value) =>
            value.text match {
              case "+"   => Variance.Covariant
              case "-"   => Variance.Contravariant
              case "in"  => Variance.Contravariant
              case "out" => Variance.Covariant
              case x     => panic("Invalid variance token: " + x)
            }
        }
        val name = identifier.text
        val symbol = scope.defineTypeParameter(
          name,
          identifier.location,
          variance
        )

        symbol match {
          case Either.Left(value) =>
            diagnosticBag.reportDuplicateDefinition(
              name,
              value,
              identifier.location
            )
            bindGenericTypeParametersWithIndex(tail, scope, index + 1)
          case Either.Right(value) =>
            val generic =
              GenericTypeParameter(
                identifier.location,
                name,
                variance,
                Option.None
              )
            setSymbolType(value, genericTypeParamAsType(generic, index))
            List.Cons(
              generic,
              bindGenericTypeParametersWithIndex(tail, scope, index + 1)
            )
        }
    }
  }

  /** Finds evidence in scope that supplies `memberName` for `leftType`.
    *
    * This is the contextual extension rule from ADR 0004: inside
    * `def same[T: Eq](a: T, b: T)`, `a.equals(b)` resolves `equals` through the
    * applicable `Eq[T]` evidence. The method does not become a member of `T` —
    * `T` has no members at all — so this only fires where an ordinary lookup
    * could never have succeeded.
    *
    * Evidence is an ordinary symbol in a scope, so this is the same upward walk
    * every other name uses, which is the practical argument ADR 0004 gives for
    * the lexical mechanism.
    */
  def findEvidenceMember(
      leftType: Type,
      memberName: string,
      symbol: Symbol
  ): Option[KeyValue[Symbol, Symbol]] = {
    scanEvidenceMembers(leftType, memberName, symbol.members()) match {
      case Option.Some(found) => Option.Some(found)
      case Option.None =>
        symbol.parent match {
          case Option.None         => Option.None
          case Option.Some(parent) =>
            findEvidenceMember(leftType, memberName, parent)
        }
    }
  }

  def scanEvidenceMembers(
      leftType: Type,
      memberName: string,
      members: List[Symbol]
  ): Option[KeyValue[Symbol, Symbol]] = {
    members match {
      case List.Nil => Option.None
      case List.Cons(head, tail) =>
        // Evidence reaches a body two ways: as its own parameter, or as the
        // field a constrained class stores its constructor's evidence in. The
        // test is on the type rather than the kind, so both are found — what
        // makes a symbol evidence is that it holds a trait applied to a type,
        // not which slot it arrived in.
        //
        // annotated: the self-hosted compiler does not widen the branches of
        // an `if` to their common supertype
        val found: Option[KeyValue[Symbol, Symbol]] =
          if (
            head.kind != SymbolKind.Evidence && head.kind != SymbolKind.Field
          ) Option.None
          else
            tryGetSymbolType(head) match {
              case Option.Some(
                    Type.Class(_, _, _, List.Cons(arg, List.Nil), traitSymbol)
                  ) =>
                if (
                  traitSymbol.kind == SymbolKind.Trait &&
                  sameConstraint(arg, leftType)
                ) {
                  traitSymbol.lookupMember(memberName) match {
                    case Option.None => Option.None
                    case Option.Some(member) =>
                      Option.Some(KeyValue(head, member))
                  }
                } else Option.None
              case _ => Option.None
            }

        found match {
          case Option.Some(_) => found
          case Option.None =>
            scanEvidenceMembers(leftType, memberName, tail)
        }
    }
  }

  /** Turns each context bound into a parameter carrying its evidence.
    *
    * Defined after the declared parameters, so `symbol.members()` puts them
    * last and both `getMethodParameterMap` and metadata emission give them the
    * trailing argument slots. Appending rather than prepending is what keeps
    * every declared parameter at the slot it already had — the VM reserves slot
    * 0 for the receiver and numbers declared parameters from 1
    * ([ADR 0005](../../../docs/architecture/adr/0005-evidence-representation.md),
    * decision A).
    *
    * Order is type-parameter declaration order, which `bindGenericConstraints`
    * already produced, and has to stay deterministic: stage 3 compares bytecode
    * from two compilers, and a different order is different bytecode.
    */
  def defineEvidenceParameters(
      constraints: List[Type],
      generics: List[GenericTypeParameter],
      location: TextLocation,
      scope: Scope
  ): List[Symbol] = {
    constraints match {
      case List.Nil => List.Nil
      case List.Cons(constraint, tail) =>
        // define before recursing: `symbol.members()` is in definition order,
        // and that order is the argument slot order
        val defined =
          scope.defineEvidence(evidenceName(constraint, generics), location)
        val rest = defineEvidenceParameters(tail, generics, location, scope)
        defined match {
          case Either.Left(_) => rest
          case Either.Right(symbol) =>
            setSymbolType(symbol, constraint)
            List.Cons(symbol, rest)
        }
    }
  }

  /** The field half of decision B: a constrained class stores the evidence its
    * constructor received, so instance methods reach it through `this` rather
    * than resolving per call. The value is resolved once, at the `new` site,
    * where the type arguments are concrete.
    */
  def defineEvidenceFields(
      constraints: List[Type],
      generics: List[GenericTypeParameter],
      location: TextLocation,
      scope: Scope
  ): unit = {
    constraints match {
      case List.Nil => ()
      case List.Cons(constraint, tail) =>
        scope.defineField(
          evidenceName(constraint, generics),
          location,
          true
        ) match {
          case Either.Left(_) =>
          case Either.Right(symbol) =>
            setSymbolType(symbol, constraint)
        }
        defineEvidenceFields(tail, generics, location, scope)
    }
  }

  /** `$ev$K$Eq` for `[K: Eq]`. Derived from the declaration rather than
    * numbered, so the name says which bound it carries.
    */
  def evidenceName(
      constraint: Type,
      generics: List[GenericTypeParameter]
  ): string = {
    constraint match {
      case Type.Class(_, _, traitName, args, _) =>
        val parameter = args match {
          case List.Cons(Type.Variable(_, id), _) =>
            genericNameAt(generics, id) match {
              case Option.Some(name) => name
              case Option.None       => string(id)
            }
          case _ => ""
        }
        "$ev$" + parameter + "$" + traitName
      case _ => "$ev$" + constraint.toString()
    }
  }

  /** How deep a chain of conditional givens may go before the compiler calls it
    * divergent. `Ord[Box[Box[int]]]` is depth 3; anything approaching this is a
    * given whose premise is no smaller than its head.
    */
  val maxEvidenceDepth: int = 32

  /** Discharges the constraints of a call to a constrained generic.
    *
    * The goals are the callee's context bounds with the call's type arguments
    * substituted in, which is what makes storing constraints applied to their
    * type variable pay off: `Eq[$0]` with `[int]` is `Eq[int]`, the goal
    * directly.
    */
  def requireEvidence(
      constraints: List[Type],
      typeArgs: List[Type],
      location: TextLocation,
      scope: Scope
  ): unit = {
    constraints match {
      case List.Nil => ()
      case List.Cons(constraint, tail) =>
        val goal = Types.substitute(constraint, typeArgs)
        if (isGroundType(goal)) {
          resolveEvidence(goal, location, 0)
          ()
        } else if (!hasEnclosingConstraint(goal, scope.current)) {
          // A goal mentioning a type variable is discharged by the enclosing
          // generic forwarding its own evidence (ADR 0005, decision B) — but
          // only if the enclosing generic actually declared it. Without this
          // check `def outer[T](a: T) = same(a, a)` would pass with nothing to
          // forward.
          diagnosticBag.reportUnconstrainedTypeParameter(
            location,
            renderGoal(goal, enclosingGenerics(scope.current))
          )
        }
        requireEvidence(tail, typeArgs, location, scope)
    }
  }

  /** The type parameters of the nearest enclosing generic declaration, so a
    * diagnostic can say `Eq[T]` rather than leaking the internal `Eq<$0>`.
    */
  def enclosingGenerics(symbol: Symbol): List[GenericTypeParameter] = {
    tryGetSymbolType(symbol) match {
      case Option.Some(Type.GenericFunction(_, generics, _, _, _)) => generics
      case _ =>
        symbol.parent match {
          case Option.None         => List.Nil
          case Option.Some(parent) => enclosingGenerics(parent)
        }
    }
  }

  def renderGoal(typ: Type, generics: List[GenericTypeParameter]): string = {
    typ match {
      case Type.Variable(_, id) =>
        genericNameAt(generics, id) match {
          case Option.Some(name) => name
          case Option.None       => typ.toString()
        }
      case Type.Class(_, _, name, args, _) =>
        if (args.isEmpty) name
        else name + "[" + renderGoalList(args, generics, "") + "]"
      case _ => typ.toString()
    }
  }

  def renderGoalList(
      types: List[Type],
      generics: List[GenericTypeParameter],
      acc: string
  ): string = {
    types match {
      case List.Nil => acc
      case List.Cons(head, tail) =>
        val separator = if (acc == "") "" else ", "
        renderGoalList(tail, generics, acc + separator + renderGoal(head, generics))
    }
  }

  def genericNameAt(
      generics: List[GenericTypeParameter],
      index: int
  ): Option[string] = {
    generics match {
      case List.Nil => Option.None
      case List.Cons(head, tail) =>
        if (index == 0) Option.Some(head.name)
        else if (index < 0) Option.None
        else genericNameAt(tail, index - 1)
    }
  }

  /** Whether some enclosing declaration already requires `goal`, and so will
    * have evidence for it to pass down.
    */
  def hasEnclosingConstraint(goal: Type, symbol: Symbol): bool = {
    val declared = tryGetSymbolType(symbol) match {
      case Option.Some(Type.GenericFunction(_, _, traits, _, _)) => traits
      case _                                                     => List.Nil
    }

    if (constraintListContains(declared, goal)) true
    else
      symbol.parent match {
        case Option.None         => false
        case Option.Some(parent) => hasEnclosingConstraint(goal, parent)
      }
  }

  def constraintListContains(constraints: List[Type], goal: Type): bool = {
    constraints match {
      case List.Nil => false
      case List.Cons(head, tail) =>
        sameConstraint(head, goal) || constraintListContains(tail, goal)
    }
  }

  /** Structural comparison that ignores source locations but, unlike
    * `typesOverlap`, holds type variables to their index. `Eq[$0]` and `Eq[$1]`
    * constrain different parameters and are not interchangeable.
    */
  def sameConstraint(left: Type, right: Type): bool = {
    Tuple2(left, right) match {
      case Tuple2(Type.Variable(_, leftId), Type.Variable(_, rightId)) =>
        leftId == rightId
      case Tuple2(
            Type.Class(_, _, leftName, leftArgs, leftSymbol),
            Type.Class(_, _, rightName, rightArgs, rightSymbol)
          ) =>
        leftSymbol == rightSymbol && leftName == rightName &&
        sameConstraintList(leftArgs, rightArgs)
      case _ => left == right
    }
  }

  def sameConstraintList(left: List[Type], right: List[Type]): bool = {
    Tuple2(left, right) match {
      case Tuple2(List.Nil, List.Nil) => true
      case Tuple2(List.Cons(l, lt), List.Cons(r, rt)) =>
        sameConstraint(l, r) && sameConstraintList(lt, rt)
      case _ => false
    }
  }

  def isGroundType(typ: Type): bool = {
    typ match {
      case Type.Variable(_, _)         => false
      case Type.Class(_, _, _, args, _) => isGroundTypeList(args)
      case Type.Alias(_, _, _, args, _, _) => isGroundTypeList(args)
      case Type.Union(_, cases)        => isGroundTypeList(cases)
      case _                           => true
    }
  }

  def isGroundTypeList(types: List[Type]): bool = {
    types match {
      case List.Nil              => true
      case List.Cons(head, tail) => isGroundType(head) && isGroundTypeList(tail)
    }
  }

  /** Finds the given that proves `goal`, and recursively the evidence its own
    * constraints need. Reports and returns `None` when there is none.
    *
    * There is no ambiguity to resolve: coherence already rejected any two
    * givens whose heads could unify, so the first match is the only match.
    */
  def resolveEvidence(
      goal: Type,
      location: TextLocation,
      depth: int
  ): Option[Evidence] = {
    if (depth > maxEvidenceDepth) {
      diagnosticBag.reportEvidenceTooDeep(location, goal.toString())
      Option.None
    } else {
      matchAnyGiven(goal, givens) match {
        case Option.None =>
          diagnosticBag.reportNoGivenInstance(location, goal.toString())
          Option.None
        case Option.Some(KeyValue(candidate, typeArgs)) =>
          resolveDependencies(
            Types.substituteList(candidate.constraints, typeArgs),
            location,
            depth + 1,
            List.Nil
          ) match {
            case Option.None => Option.None
            case Option.Some(dependencies) =>
              Option.Some(
                Evidence(candidate.symbol, goal, typeArgs, dependencies)
              )
          }
      }
    }
  }

  def resolveDependencies(
      goals: List[Type],
      location: TextLocation,
      depth: int,
      acc: List[Evidence]
  ): Option[List[Evidence]] = {
    goals match {
      case List.Nil => Option.Some(acc.reverse())
      case List.Cons(goal, tail) =>
        resolveEvidence(goal, location, depth) match {
          case Option.None => Option.None
          case Option.Some(evidence) =>
            resolveDependencies(
              tail,
              location,
              depth,
              List.Cons(evidence, acc)
            )
        }
    }
  }

  /** The first given whose head matches `goal`, with the type arguments that
    * make it match.
    */
  def matchAnyGiven(
      goal: Type,
      candidates: List[BoundGiven]
  ): Option[KeyValue[BoundGiven, List[Type]]] = {
    candidates match {
      case List.Nil => Option.None
      case List.Cons(candidate, tail) =>
        // annotated: the self-hosted compiler infers `Dictionary<any, any>`
        // for an empty dictionary in argument position
        val empty: Dictionary[int, Type] = DictionaryModule.empty()
        matchType(candidate.head, goal, empty) match {
          case Option.None => matchAnyGiven(goal, tail)
          case Option.Some(bindings) =>
            orderBindings(bindings, candidate.generics.length, 0) match {
              case Option.None => matchAnyGiven(goal, tail)
              case Option.Some(typeArgs) =>
                Option.Some(KeyValue(candidate, typeArgs))
            }
        }
    }
  }

  /** One-way match: `pattern` comes from a given's head and may contain type
    * variables, `goal` is ground. Unlike `typesOverlap`, which only answers
    * yes or no for the coherence check, this records what each variable had to
    * be — a conditional given needs those bindings to instantiate its premise.
    */
  def matchType(
      pattern: Type,
      goal: Type,
      bindings: Dictionary[int, Type]
  ): Option[Dictionary[int, Type]] = {
    Tuple2(pattern, goal) match {
      case Tuple2(Type.Variable(_, id), _) =>
        bindings.get(id) match {
          case Option.None => Option.Some(bindings.put(id, goal))
          case Option.Some(bound) =>
            if (bound == goal) Option.Some(bindings) else Option.None
        }
      case Tuple2(
            Type.Class(_, _, leftName, leftArgs, leftSymbol),
            Type.Class(_, _, rightName, rightArgs, rightSymbol)
          ) =>
        if (leftSymbol == rightSymbol && leftName == rightName) {
          matchTypeList(leftArgs, rightArgs, bindings)
        } else Option.None
      case _ =>
        if (pattern == goal) Option.Some(bindings) else Option.None
    }
  }

  def matchTypeList(
      patterns: List[Type],
      goals: List[Type],
      bindings: Dictionary[int, Type]
  ): Option[Dictionary[int, Type]] = {
    Tuple2(patterns, goals) match {
      case Tuple2(List.Nil, List.Nil) => Option.Some(bindings)
      case Tuple2(List.Cons(pattern, patternTail), List.Cons(goal, goalTail)) =>
        matchType(pattern, goal, bindings) match {
          case Option.None => Option.None
          case Option.Some(next) =>
            matchTypeList(patternTail, goalTail, next)
        }
      case _ => Option.None
    }
  }

  /** Turns the bindings into positional type arguments. `Option.None` when a
    * type parameter never appeared in the head, which leaves nothing to
    * instantiate it with.
    */
  def orderBindings(
      bindings: Dictionary[int, Type],
      count: int,
      index: int
  ): Option[List[Type]] = {
    if (index >= count) Option.Some(List.Nil)
    else {
      bindings.get(index) match {
        case Option.None => Option.None
        case Option.Some(typ) =>
          orderBindings(bindings, count, index + 1) match {
            case Option.None       => Option.None
            case Option.Some(rest) => Option.Some(List.Cons(typ, rest))
          }
      }
    }
  }

  /** An evidence record is an `Array[int]` of method tokens, one slot per trait
    * member, indexed by the member's position in the trait.
    *
    * ADR 0005 calls it a record of method tokens; an int array is that on this
    * VM without synthesizing a type and a constructor for every given. A slot
    * per member — rather than one field per member with the record skipped —
    * is what keeps the shape the same for a trait with any number of members.
    */
  def defineEvidenceRecords(
      remaining: List[BoundGiven],
      program: Symbol
  ): unit = {
    remaining match {
      case List.Nil => ()
      case List.Cons(head, tail) =>
        program.tryDefineField(head.symbol.name, head.location, true) match {
          case Either.Left(_) =>
          case Either.Right(field) =>
            field.extern = false
            setSymbolType(field, evidenceRecordType)
            evidenceFields = evidenceFields.put(head.symbol, field)
        }
        defineEvidenceRecords(tail, program)
    }
  }

  val evidenceRecordType: Type =
    Type.Class(
      noLoc,
      List.Nil,
      "Array",
      ListModule.one(intType),
      arraySymbol
    )

  /** A trait's callable members, in declaration order. That order is the
    * evidence record's layout, so it has to be the same on both sides — the
    * given that fills a slot and the call that reads it.
    */
  def traitMembers(traitSymbol: Symbol): List[Symbol] =
    filterMethods(traitSymbol.members())

  def filterMethods(symbols: List[Symbol]): List[Symbol] = {
    symbols match {
      case List.Nil => List.Nil
      case List.Cons(head, tail) =>
        if (head.kind == SymbolKind.Method)
          List.Cons(head, filterMethods(tail))
        else filterMethods(tail)
    }
  }

  def indexOfSymbol(symbols: List[Symbol], target: Symbol, index: int): int = {
    symbols match {
      case List.Nil => -1
      case List.Cons(head, tail) =>
        if (head == target) index
        else indexOfSymbol(tail, target, index + 1)
    }
  }

  // ── Derivation ──────────────────────────────────────────────────────────
  //
  // What each trait means over a type's constructor parameters, in declaration
  // order ([ADR 0004](../../../docs/architecture/adr/0004-traits-given-evidence-and-contextual-extensions.md)):
  //
  //   Eq    reference identity first, then parameter by parameter
  //   Ord   lexicographic over the parameters, in order
  //   Show  "Name(" + show(p1) + ", " + … + ")"
  //
  // Constructor parameters only. A `var` in the class body does not
  // participate, which is not a detail: `Symbol._children` is such a field, it
  // points back at parents, and a derived `Eq` that walked it would not
  // terminate.

  def buildDerivedBodies(remaining: List[Derivation]): unit = {
    remaining match {
      case List.Nil => ()
      case List.Cons(head, tail) =>
        buildDerivedBody(head)
        buildDerivedBodies(tail)
    }
  }

  def buildDerivedBody(derivation: Derivation): unit = {
    val fields = derivedFields(derivation.typeSymbol, derivation.parameterNames)

    if (derivation.traitSymbol == eqSymbol) {
      buildDerivedEq(derivation, fields)
    } else if (derivation.traitSymbol == ordSymbol) {
      buildDerivedOrd(derivation, fields)
    } else {
      buildDerivedShow(derivation, fields)
    }
  }

  /** The fields the constructor parameters became, in declaration order. */
  def derivedFields(
      typeSymbol: Symbol,
      names: List[string]
  ): List[Symbol] = {
    names match {
      case List.Nil => List.Nil
      case List.Cons(head, tail) =>
        val rest = derivedFields(typeSymbol, tail)
        typeSymbol.lookupMember(head) match {
          case Option.Some(field) => List.Cons(field, rest)
          case Option.None        => rest
        }
    }
  }

  def derivedMemberOf(givenSymbol: Symbol, name: string): Symbol = {
    givenSymbol.lookupMember(name) match {
      case Option.Some(member) => member
      case Option.None =>
        panic("buildDerivedBody: " + givenSymbol.name + " has no " + name)
    }
  }

  def filterParameters(symbols: List[Symbol]): List[Symbol] = {
    symbols match {
      case List.Nil => List.Nil
      case List.Cons(head, tail) =>
        if (head.kind == SymbolKind.Parameter)
          List.Cons(head, filterParameters(tail))
        else filterParameters(tail)
    }
  }

  /** The two operands of a derived binary member, which
    * `builtinBinaryMember` defined as `a` and `b`.
    */
  def derivedOperands(member: Symbol): Tuple2[Symbol, Symbol] = {
    filterParameters(member.members()) match {
      case List.Cons(a, List.Cons(b, _)) => Tuple2(a, b)
      case _ => panic("buildDerivedBody: " + member.name + " is not binary")
    }
  }

  def derivedVariable(symbol: Symbol, typ: Type): BoundExpression =
    BoundExpression.Variable(noLoc, symbol, Option.Some(typ))

  /** `receiver.field` */
  def derivedFieldAccess(receiver: Symbol, field: Symbol): BoundExpression =
    BoundExpression.MemberAccess(
      noLoc,
      BoundLeftHandSide.Variable(noLoc, receiver),
      field,
      List.Nil,
      getSymbolType(field)
    )

  def derivedCall(
      member: Symbol,
      arguments: List[BoundExpression],
      result: Type
  ): BoundExpression =
    BoundExpression.Call(noLoc, Option.None, member, List.Nil, arguments, result)

  def derivedCall2(
      member: Symbol,
      left: BoundExpression,
      right: BoundExpression,
      result: Type
  ): BoundExpression =
    derivedCall(member, List.Cons(left, ListModule.one(right)), result)

  /** The member of the given that proves `traitSymbol[typ]`.
    *
    * Derivation requires evidence for every parameter type and names the one
    * that lacks it. There is no recursion guard to add: the derived given is
    * registered before any body is built, so a type whose parameter is itself
    * — `Eq[Symbol]` needing `Eq[Symbol]` — finds the given it is inside.
    */
  def derivedEvidence(
      traitSymbol: Symbol,
      field: Symbol,
      memberName: string
  ): Option[Symbol] = {
    val goal: Type =
      Type.Class(
        noLoc,
        List.Nil,
        traitSymbol.name,
        ListModule.one(getSymbolType(field)),
        traitSymbol
      )

    findGivenMember(goal, memberName) match {
      case Option.Some(member) => Option.Some(member)
      case Option.None =>
        diagnosticBag.reportNoEvidenceForDerivedField(
          field.location,
          field.name,
          traitSymbol.name
        )
        Option.None
    }
  }

  def buildDerivedEq(derivation: Derivation, fields: List[Symbol]): unit = {
    val equals = derivedMemberOf(derivation.givenSymbol, "==")
    val notEquals = derivedMemberOf(derivation.givenSymbol, "!=")

    derivedOperands(equals) match {
      case Tuple2(a, b) =>
        // Reference identity first, and it is part of the shape rather than an
        // optimisation: a `Symbol` holds a `TextLocation`, which holds the
        // whole text of a file, and a purely structural `Eq[Symbol]` would
        // compare two files on every dictionary lookup (ADR 0004).
        val identity: BoundExpression =
          BoundExpression.Binary(
            noLoc,
            derivedVariable(a, derivation.typ),
            BinaryOperatorKind.Equals,
            derivedVariable(b, derivation.typ),
            boolType
          )

        functionBodies = functionBodies.put(
          equals,
          BoundExpression.If(
            noLoc,
            identity,
            BoundExpression.Boolean(noLoc, true),
            Option.Some(derivedEqChain(fields, a, b)),
            boolType
          )
        )
    }

    derivedOperands(notEquals) match {
      case Tuple2(a, b) =>
        functionBodies = functionBodies.put(
          notEquals,
          BoundExpression.Unary(
            noLoc,
            UnaryOperatorKind.LogicalNegation,
            derivedCall2(
              equals,
              derivedVariable(a, derivation.typ),
              derivedVariable(b, derivation.typ),
              boolType
            ),
            boolType
          )
        )
    }
  }

  /** `a.p1 == b.p1 && a.p2 == b.p2 && …`, and `true` for a type with no
    * constructor parameters — two of those are equal whenever they exist.
    */
  def derivedEqChain(
      fields: List[Symbol],
      a: Symbol,
      b: Symbol
  ): BoundExpression = {
    fields match {
      case List.Nil => BoundExpression.Boolean(noLoc, true)
      case List.Cons(field, tail) =>
        val comparison = derivedFieldEquals(field, a, b)
        if (tail.isEmpty) comparison
        else
          BoundExpression.Binary(
            noLoc,
            comparison,
            BinaryOperatorKind.LogicalAnd,
            derivedEqChain(tail, a, b),
            boolType
          )
    }
  }

  def derivedFieldEquals(
      field: Symbol,
      a: Symbol,
      b: Symbol
  ): BoundExpression = {
    derivedEvidence(eqSymbol, field, "==") match {
      case Option.None => BoundExpression.Boolean(noLoc, false)
      case Option.Some(member) =>
        derivedCall2(
          member,
          derivedFieldAccess(a, field),
          derivedFieldAccess(b, field),
          boolType
        )
    }
  }

  def buildDerivedOrd(derivation: Derivation, fields: List[Symbol]): unit = {
    val lessThan = derivedMemberOf(derivation.givenSymbol, "<")

    derivedOperands(lessThan) match {
      case Tuple2(a, b) =>
        functionBodies =
          functionBodies.put(lessThan, derivedOrdChain(fields, a, b))
    }

    // The other three are the one comparison read differently, which is also
    // what keeps them consistent with it.
    derivedOrdDerived(derivation, lessThan, "<=", true, true)
    derivedOrdDerived(derivation, lessThan, ">", true, false)
    derivedOrdDerived(derivation, lessThan, ">=", false, true)
  }

  /** `<=` is `!(b < a)`, `>` is `b < a`, `>=` is `!(a < b)`. `swapped` says
    * which way round the operands go, `negated` whether the result is flipped.
    */
  def derivedOrdDerived(
      derivation: Derivation,
      lessThan: Symbol,
      name: string,
      swapped: bool,
      negated: bool
  ): unit = {
    val member = derivedMemberOf(derivation.givenSymbol, name)

    derivedOperands(member) match {
      case Tuple2(a, b) =>
        val left = derivedVariable(a, derivation.typ)
        val right = derivedVariable(b, derivation.typ)
        val call =
          if (swapped) derivedCall2(lessThan, right, left, boolType)
          else derivedCall2(lessThan, left, right, boolType)

        val body: BoundExpression =
          if (negated)
            BoundExpression.Unary(
              noLoc,
              UnaryOperatorKind.LogicalNegation,
              call,
              boolType
            )
          else call

        functionBodies = functionBodies.put(member, body)
    }
  }

  /** Lexicographic: the first parameter that differs decides, and a type with
    * no parameters is never less than another.
    */
  def derivedOrdChain(
      fields: List[Symbol],
      a: Symbol,
      b: Symbol
  ): BoundExpression = {
    fields match {
      case List.Nil => BoundExpression.Boolean(noLoc, false)
      case List.Cons(field, tail) =>
        val aBeforeB = derivedFieldLessThan(field, a, b)
        if (tail.isEmpty) aBeforeB
        else
          BoundExpression.If(
            noLoc,
            aBeforeB,
            BoundExpression.Boolean(noLoc, true),
            Option.Some(
              BoundExpression.If(
                noLoc,
                derivedFieldLessThan(field, b, a),
                BoundExpression.Boolean(noLoc, false),
                Option.Some(derivedOrdChain(tail, a, b)),
                boolType
              )
            ),
            boolType
          )
    }
  }

  def derivedFieldLessThan(
      field: Symbol,
      a: Symbol,
      b: Symbol
  ): BoundExpression = {
    derivedEvidence(ordSymbol, field, "<") match {
      case Option.None => BoundExpression.Boolean(noLoc, false)
      case Option.Some(member) =>
        derivedCall2(
          member,
          derivedFieldAccess(a, field),
          derivedFieldAccess(b, field),
          boolType
        )
    }
  }

  def buildDerivedShow(derivation: Derivation, fields: List[Symbol]): unit = {
    val show = derivedMemberOf(derivation.givenSymbol, "show")

    filterParameters(show.members()) match {
      case List.Cons(value, _) =>
        val opening: BoundExpression =
          BoundExpression.String(noLoc, derivation.typeSymbol.name + "(")
        val body = derivedConcat(
          derivedShowChain(fields, value, opening, true),
          BoundExpression.String(noLoc, ")")
        )
        functionBodies = functionBodies.put(show, body)
      case List.Nil => panic("buildDerivedShow: show has no parameter")
    }
  }

  def derivedConcat(
      left: BoundExpression,
      right: BoundExpression
  ): BoundExpression =
    BoundExpression.Binary(
      noLoc,
      left,
      BinaryOperatorKind.Plus,
      right,
      stringType
    )

  def derivedShowChain(
      fields: List[Symbol],
      value: Symbol,
      acc: BoundExpression,
      first: bool
  ): BoundExpression = {
    fields match {
      case List.Nil => acc
      case List.Cons(field, tail) =>
        val separated =
          if (first) acc
          else derivedConcat(acc, BoundExpression.String(noLoc, ", "))

        derivedShowChain(
          tail,
          value,
          derivedConcat(separated, derivedFieldShow(field, value)),
          false
        )
    }
  }

  def derivedFieldShow(field: Symbol, value: Symbol): BoundExpression = {
    derivedEvidence(showSymbol, field, "show") match {
      case Option.None => BoundExpression.String(noLoc, "")
      case Option.Some(member) =>
        derivedCall(
          member,
          ListModule.one(derivedFieldAccess(value, field)),
          stringType
        )
    }
  }

  def bindGivens(
      givens: List[Namespaced[MemberSyntax.GivenDeclarationSyntax]],
      scope: Scope
  ): unit = {
    givens match {
      case List.Nil => ()
      case List.Cons(head, tail) =>
        bindGiven(head, scope)
        bindGivens(tail, scope)
    }
  }

  /** Binds one `given` and registers it.
    *
    * The symbol is named after its position rather than its head: a given has
    * no user-visible name, and the head type is not known until the given's own
    * type parameters are in scope. Numbering follows source order, so it stays
    * deterministic for stage 3.
    */
  def bindGiven(
      head: Namespaced[MemberSyntax.GivenDeclarationSyntax],
      scope: Scope
  ): unit = {
    val location = AstUtils.locationOfName(head.value.name)
    val name = "$given$" + string(givenCount)
    givenCount = givenCount + 1

    scope.defineGiven(name, location) match {
      case Either.Left(existing) =>
        diagnosticBag.reportDuplicateDefinition(name, existing, location)
      case Either.Right(symbol) =>
        val givenScope = scope.enterSymbol(symbol)

        val generics: List[GenericTypeParameter] =
          head.value.genericParameters match {
            case Option.None => List.Nil
            case Option.Some(value) =>
              bindGenericTypeParameters(value.parameters.items, givenScope)
          }
        val constraints: List[Type] = head.value.genericParameters match {
          case Option.None => List.Nil
          case Option.Some(value) =>
            bindGenericConstraints(value.parameters.items, givenScope)
        }

        val headType = bindTypeName(head.value.name, givenScope)
        setSymbolType(symbol, headType)

        checkGivenHead(headType, location) match {
          case false =>
          case true =>
            registerGiven(
              BoundGiven(symbol, headType, generics, constraints, location)
            )
        }

        val members = splitMembers(List.Nil, head.value.template.members)
        bindClassesObjectAndEnums(
          members.classes,
          members.objects,
          members.enums,
          members.traits,
          members.givens,
          givenScope
        )
        addMembersToBind(
          symbol,
          members.functions,
          members.fields,
          List.Nil
        )
    }
  }

  /** A given's head has to name a trait applied to arguments. `given Foo[int]`
    * where `Foo` is a class proves nothing.
    */
  def checkGivenHead(headType: Type, location: TextLocation): bool = {
    headType match {
      case Type.Error(_) =>
        // already reported by bindTypeName
        false
      case Type.Class(_, _, name, args, symbol) =>
        if (symbol.kind != SymbolKind.Trait) {
          diagnosticBag.reportGivenHeadNotATrait(location, name)
          false
        } else if (args.isEmpty) {
          diagnosticBag.reportGivenHeadMissingArguments(location, name)
          false
        } else true
      // A generic trait named without arguments stays uninstantiated, so
      // `given Eq { … }` lands here rather than in the `args.isEmpty` branch.
      case Type.GenericClass(_, _, name, _, symbol) =>
        if (symbol.kind != SymbolKind.Trait) {
          diagnosticBag.reportGivenHeadNotATrait(location, name)
        } else {
          diagnosticBag.reportGivenHeadMissingArguments(location, name)
        }
        false
      case _ =>
        diagnosticBag.reportGivenHeadNotATrait(location, headType.toString())
        false
    }
  }

  /** Registration is global even though declaration is lexical: under global
    * coherence a given in a sibling namespace is still the only candidate, so
    * it has to be reachable. The declaration site is kept on `BoundGiven` but
    * does not limit visibility yet — restricting it is what relaxing to true
    * lexical scoping would turn on
    * ([ADR 0004](../../../docs/architecture/adr/0004-traits-given-evidence-and-contextual-extensions.md)).
    */
  def registerGiven(candidate: BoundGiven): unit = {
    findOverlappingGiven(candidate, givens) match {
      case Option.Some(existing) =>
        diagnosticBag.reportOverlappingGiven(
          candidate.location,
          candidate.head.toString(),
          existing.location
        )
      case Option.None =>
        givens = List.Cons(candidate, givens)
    }
  }

  /** The static field holding the evidence record that proves `goal`, if a
    * given does. Used by the emitter, which needs the field rather than the
    * proof tree, and reports nothing — binding already did.
    */
  def findEvidenceRecord(goal: Type): Option[Symbol] = {
    matchAnyGiven(goal, givens) match {
      case Option.None                        => Option.None
      case Option.Some(KeyValue(candidate, _)) =>
        evidenceFields.get(candidate.symbol)
    }
  }

  /** The trait that claims `tokenKind`, applied to `operandType`.
    *
    * This is the goal a use site of that operator has to prove: `a == b` on
    * `Box` needs `Eq[Box]`. Returns `None` when no trait claims the token,
    * which is the ordinary case — the builtin operator table answers for the
    * value types and nothing needs evidence.
    */
  def operatorGoal(tokenKind: int, operandType: Type): Option[Type] = {
    operatorTraits.get(tokenKind) match {
      case Option.None => Option.None
      case Option.Some(traitSymbol) =>
        tryGetSymbolType(traitSymbol) match {
          case Option.Some(
                Type.GenericClass(loc, ns, traitName, args, symbol)
              ) =>
            if (args.length == 1) {
              Option.Some(
                Type.Class(loc, ns, traitName, ListModule.one(operandType), symbol)
              )
            } else Option.None
          case _ => Option.None
        }
    }
  }

  /** The member of the given that proves `goal`, by name.
    *
    * The ground half of operator resolution: where `findEvidenceMember` finds
    * evidence held by an enclosing declaration, this finds the given itself.
    * A given is a singleton whose members are static, so the caller can emit an
    * ordinary call rather than going through the evidence record — the record
    * exists to defer a choice, and here there is nothing left to defer.
    */
  def findGivenMember(goal: Type, memberName: string): Option[Symbol] = {
    matchAnyGiven(goal, givens) match {
      case Option.None => Option.None
      case Option.Some(KeyValue(candidate, _)) =>
        candidate.symbol.lookupMember(memberName)
    }
  }

  /** The member a given supplies as a contextual extension on `leftType`.
    *
    * The ground-type half of ADR 0004's member resolution: intrinsic members
    * first, then applicable contextual extensions. Where `findEvidenceMember`
    * answers for a type parameter — whose evidence an enclosing declaration
    * holds — this answers for a type the givens name directly, so
    * `value.show()` works on a `Point` and not only inside `[T: Show]`.
    *
    * Only reached once an ordinary lookup has failed, which is what settles
    * the shadowing question: a member a type declares itself always wins.
    */
  def findGivenExtension(
      leftType: Type,
      memberName: string
  ): Option[Symbol] = scanGivenExtensions(leftType, memberName, givens)

  def scanGivenExtensions(
      leftType: Type,
      memberName: string,
      candidates: List[BoundGiven]
  ): Option[Symbol] = {
    candidates match {
      case List.Nil => Option.None
      case List.Cons(candidate, tail) =>
        val found: Option[Symbol] = candidate.head match {
          case Type.Class(_, _, _, List.Cons(arg, List.Nil), traitSymbol) =>
            if (
              traitSymbol.kind == SymbolKind.Trait &&
              sameConstraint(arg, leftType)
            ) candidate.symbol.lookupMember(memberName)
            else Option.None
          case _ => Option.None
        }

        found match {
          case Option.Some(_) => found
          case Option.None =>
            scanGivenExtensions(leftType, memberName, tail)
        }
    }
  }

  def findOverlappingGiven(
      candidate: BoundGiven,
      existing: List[BoundGiven]
  ): Option[BoundGiven] = {
    existing match {
      case List.Nil => Option.None
      case List.Cons(head, tail) =>
        if (typesOverlap(candidate.head, head.head)) Option.Some(head)
        else findOverlappingGiven(candidate, tail)
    }
  }

  /** Whether two given heads could ever denote the same instance.
    *
    * Overlap is unification, not equality: `Ord[List[T]]` and `Ord[List[int]]`
    * are two givens for one pair the moment `T` can be `int`, and comparing
    * arguments structurally would let exactly that pair through. A
    * `Type.Variable` therefore matches anything — which is sound in the
    * rejecting direction and is all the coherence rule needs, since there is no
    * specificity contest to win.
    */
  def typesOverlap(left: Type, right: Type): bool = {
    Tuple2(left, right) match {
      case Tuple2(Type.Variable(_, _), _) => true
      case Tuple2(_, Type.Variable(_, _)) => true
      case Tuple2(
            Type.Class(_, _, leftName, leftArgs, leftSymbol),
            Type.Class(_, _, rightName, rightArgs, rightSymbol)
          ) =>
        leftSymbol == rightSymbol && leftName == rightName &&
        typeListsOverlap(leftArgs, rightArgs)
      case _ => left == right
    }
  }

  def typeListsOverlap(left: List[Type], right: List[Type]): bool = {
    Tuple2(left, right) match {
      case Tuple2(List.Nil, List.Nil) => true
      case Tuple2(List.Cons(l, lt), List.Cons(r, rt)) =>
        typesOverlap(l, r) && typeListsOverlap(lt, rt)
      case _ => false
    }
  }

  /** The context bounds of a generic parameter list, as constraint types.
    *
    * `[K: Eq, V]` yields `Eq[$0]`, where `$0` is the type variable standing for
    * `K`. Applying the trait to the variable rather than recording the pair
    * separately is what lets `Types.substitute` do the work later: substituting
    * `int` for `$0` turns the constraint into `Eq[int]`, which is exactly the
    * goal evidence resolution has to satisfy
    * ([ADR 0005](../../../docs/architecture/adr/0005-evidence-representation.md),
    * decision E).
    *
    * Run this after `bindGenericTypeParameters`, which is what defines the
    * parameters and fixes their indices.
    */
  def bindGenericConstraints(
      value: List[GenericParameterSyntax],
      scope: Scope
  ): List[Type] = bindGenericConstraintsWithIndex(value, scope, 0)

  def bindGenericConstraintsWithIndex(
      value: List[GenericParameterSyntax],
      scope: Scope,
      index: int
  ): List[Type] = {
    value match {
      case List.Nil => List.Nil
      case List.Cons(
            GenericParameterSyntax(_, identifier, bounds),
            tail
          ) =>
        val rest = bindGenericConstraintsWithIndex(tail, scope, index + 1)
        bounds match {
          case Option.None => rest
          case Option.Some(GenericBoundsSyntax(_, name)) =>
            bindContextBound(name, identifier.location, index, scope) match {
              case Option.None            => rest
              case Option.Some(constraint) => List.Cons(constraint, rest)
            }
        }
    }
  }

  /** Resolves the trait named by one context bound and applies it to the
    * constrained parameter. Returns `Option.None` when the bound is unusable,
    * having reported why; the declaration still binds, just without that
    * constraint.
    */
  def bindContextBound(
      name: NameSyntax,
      parameterLocation: TextLocation,
      index: int,
      scope: Scope
  ): Option[Type] = {
    val location = AstUtils.locationOfName(name)
    lookupContextBound(name, scope) match {
      case Option.None => Option.None
      case Option.Some(symbol) =>
        if (symbol.kind != SymbolKind.Trait) {
          diagnosticBag.reportContextBoundNotATrait(location, symbol.name)
          Option.None
        } else {
          val variable = Type.Variable(parameterLocation, index)
          tryGetSymbolType(symbol) match {
            case Option.Some(
                  Type.GenericClass(loc, ns, traitName, args, traitSymbol)
                ) =>
              if (args.length == 1) {
                Option.Some(
                  Type.Class(
                    loc,
                    ns,
                    traitName,
                    List.Cons(variable, List.Nil),
                    traitSymbol
                  )
                )
              } else {
                diagnosticBag.reportContextBoundArity(
                  location,
                  traitName,
                  args.length
                )
                Option.None
              }
            case _ =>
              // A trait with no type parameters. `K: Show` would mean
              // `Show[K]`, and there is no slot for `K`.
              diagnosticBag.reportContextBoundArity(location, symbol.name, 0)
              Option.None
          }
        }
    }
  }

  def lookupContextBound(name: NameSyntax, scope: Scope): Option[Symbol] = {
    name match {
      case NameSyntax.SimpleName(simple) =>
        lookupContextBoundSimpleName(simple, scope)
      case NameSyntax.QualifiedName(left, _, right) =>
        lookupContextBoundSimpleName(right, bindNameToScope(left, scope))
    }
  }

  def lookupContextBoundSimpleName(
      name: SimpleNameSyntax,
      scope: Scope
  ): Option[Symbol] = {
    name match {
      case SimpleNameSyntax.IdentifierNameSyntax(identifier) =>
        scope.lookup(identifier.text) match {
          case Option.None =>
            diagnosticBag.reportTypeNotDefined(
              identifier.location,
              identifier.text
            )
            Option.None
          case symbol => symbol
        }
      case _ =>
        // `K: Eq[int]` and the alias forms. The constrained parameter is the
        // only argument a context bound can take, so it is never written.
        diagnosticBag.reportContextBoundNotATrait(
          AstUtils.locationOfSimpleName(name),
          "the bound"
        )
        Option.None
    }
  }

  def bindObjects(
      objects: List[Namespaced[MemberSyntax.ObjectDeclarationSyntax]],
      parentScope: Scope
  ): List[BoundDefinition] = {
    objects match {
      case List.Nil => List.Nil
      case List.Cons(head, tail) =>
        bindObject(head, parentScope)
        bindObjects(tail, parentScope)
    }
  }

  def bindObject(
      head: Namespaced[MemberSyntax.ObjectDeclarationSyntax],
      parentScope: Scope
  ): unit = {
    val scope = getNamespacedScope(head.ns, parentScope)
    val name = head.value.identifier.text
    scope.defineObject(
      name,
      head.value.identifier.location
    ) match {
      case Either.Left(location) =>
        diagnosticBag.reportDuplicateDefinition(
          name,
          location,
          head.value.identifier.location
        )
      case Either.Right(symbol) =>
        // no namespace here as the scope is already namespaced
        val members = splitMembers(List.Nil, head.value.template.members)
        bindClassesObjectAndEnums(
          members.classes,
          members.objects,
          members.enums,
          members.traits,
          members.givens,
          scope.enterSymbol(symbol)
        )
        setSymbolType(
          symbol,
          new Type.Class(
            symbol.location,
            symbol.ns(),
            symbol.name,
            List.Nil,
            symbol
          )
        )

        // Convert field declarations with initializers into assignment statements
        // These will be executed during object initialization in $runtimeInit
        val fieldInitStatements = fieldsToInitStatements(members.fields)
        val allStatements =
          appendStatements(members.globalStatements, fieldInitStatements)

        addMembersToBind(symbol, members.functions, members.fields, List.Nil)
        addStatementsToBind(symbol, allStatements)

        // Register this object for static constructor creation if it has initialization statements or fields
        if (allStatements.length > 0 || members.fields.length > 0) {
          ctorsToBind =
            ctorsToBind.put(
              symbol,
              ConstructorParams(List.Nil, List.Nil, List.Nil)
            )
        }
    }
  }

  def addMembersToBind(
      symbol: Symbol,
      functions: List[MemberSyntax.FunctionDeclarationSyntax],
      fields: List[MemberSyntax.VariableDeclaration],
      parameters: List[ParameterSyntax]
  ): unit = {
    val members =
      variablesToMembers(
        functionsToMembers(
          parametersToMembers(List.Nil, parameters),
          functions
        ),
        fields
      )

    if (members.length > 0) {
      membersToBind = membersToBind.put(symbol, members)
    }
  }

  def parametersToMembers(
      members: List[BindingMember],
      parameters: List[ParameterSyntax]
  ): List[BindingMember] = {
    parameters match {
      case List.Nil => members
      case List.Cons(head, tail) =>
        parametersToMembers(
          List.Cons(BindingMember.Parameter(head), members),
          tail
        )
    }
  }

  def functionsToMembers(
      members: List[BindingMember],
      functions: List[MemberSyntax.FunctionDeclarationSyntax]
  ): List[BindingMember] = {
    functions match {
      case List.Nil => members
      case List.Cons(head, tail) =>
        functionsToMembers(List.Cons(BindingMember.Method(head), members), tail)
    }
  }

  def variablesToMembers(
      members: List[BindingMember],
      variables: List[MemberSyntax.VariableDeclaration]
  ): List[BindingMember] = {
    variables match {
      case List.Nil => members
      case List.Cons(head, tail) =>
        variablesToMembers(
          List.Cons(BindingMember.Field(head), members),
          tail
        )
    }
  }

  def addStatementsToBind(
      symbol: Symbol,
      statements: List[MemberSyntax.GlobalStatementSyntax]
  ): unit = {
    // ensure we add statements to bind for clases even when we dont have any to
    // ensure that a constructor is created for classes that have no statements
    if (statements.length > 0 || symbol.kind == SymbolKind.Class) {
      statementsToBind = statementsToBind.put(symbol, statements)
    }
  }

  def getNamespacedScope(
      ns: List[string],
      scope: Scope
  ): Scope = {
    ns match {
      case List.Nil => scope
      case List.Cons(head, tail) =>
        getNamespacedScope(tail, scope.enter(head))
    }
  }

  def removeProgram(
      members: List[Namespaced[MemberSyntax.ObjectDeclarationSyntax]]
  ): List[Namespaced[MemberSyntax.ObjectDeclarationSyntax]] = {
    members match {
      case List.Nil => List.Nil
      case List.Cons(head, tail) =>
        if (head.value.identifier.text == "Program") {
          tail
        } else {
          List.Cons(head, removeProgram(tail))
        }
    }
  }

  def removeMain(
      members: List[MemberSyntax.FunctionDeclarationSyntax]
  ): List[MemberSyntax.FunctionDeclarationSyntax] = {
    members match {
      case List.Nil => List.Nil
      case List.Cons(head, tail) =>
        if (head.identifier.text == "main") {
          tail
        } else {
          List.Cons(head, removeMain(tail))
        }
    }
  }

  def getMainMethod(
      program: Symbol
  ): Symbol = {
    program.lookup("main") match {
      case Option.None =>
        // no main method so lets create one
        val location = noLoc
        val main = program.defineMethod("main", location)
        setSymbolType(main, Type.Function(location, List.Nil, unitType))
        main
      case Option.Some(symbol) =>
        // TODO: we should probably verify that this is a Method symbol
        symbol
    }
  }

  def getRuntimeInit(program: Symbol): Symbol = {
    // let's create runtime init symbol
    val location = noLoc
    val init = program.defineMethod("$runtimeInit", location)
    setSymbolType(init, Type.Function(location, List.Nil, unitType))
    init
  }

  def getProgramSymbol(root: Symbol): Symbol = {
    root.lookup("Program") match {
      case Option.None =>
        // no program symbol so lets create one
        root.defineObject("$Program", noLoc)
      case Option.Some(symbol) =>
        // TODO: we should probably verify that this is a Object symbol
        symbol
    }
  }

  /** Split members into disparate lists from the given list of syntax trees.
    *
    *   - variable declarations generate assignments as statements
    *   - this does not perform nested extraction
    */
  def splitMembersInTrees(trees: List[SyntaxTree]): Members = {
    _splitMembers(
      List.Nil,
      trees,
      List.Nil,
      List.Nil,
      List.Nil,
      List.Nil,
      List.Nil,
      List.Nil,
      List.Nil,
      List.Nil,
      List.Nil
    )
  }

  def splitMembers(ns: List[string], members: List[MemberSyntax]): Members = {
    _splitMembers(
      ns,
      List.Nil,
      members,
      List.Nil,
      List.Nil,
      List.Nil,
      List.Nil,
      List.Nil,
      List.Nil,
      List.Nil,
      List.Nil
    )
  }

  def simpleNameToNamespace(name: SimpleNameSyntax): string = {
    name match {
      case SimpleNameSyntax.GenericNameSyntax(identifier, typeArgumentlist) =>
        diagnosticBag.reportInvalidNamespace(
          identifier.location.merge(typeArgumentlist.greaterThanToken.location)
        )
        identifier.text
      case SimpleNameSyntax.IdentifierNameSyntax(identifier) =>
        identifier.text
      case SimpleNameSyntax.ScalaAliasSyntax(open, name, arrow, alias, close) =>
        diagnosticBag.reportInvalidNamespace(
          name.location.merge(close.location)
        )
        name.text
      case SimpleNameSyntax.AliasSyntax(name, asKeyword, alias) =>
        diagnosticBag.reportInvalidNamespace(
          name.location.merge(alias.location)
        )
        name.text
    }
  }

  def nameToNamespace(name: NameSyntax, ns: List[string]): List[string] = {
    name match {
      case NameSyntax.SimpleName(value) =>
        List.Cons(simpleNameToNamespace(value), ns).reverse()
      case NameSyntax.QualifiedName(left, _, right) =>
        nameToNamespace(left, List.Cons(simpleNameToNamespace(right), ns))
    }
  }

  def _splitMembers(
      ns: List[string],
      trees: List[SyntaxTree],
      members: List[MemberSyntax],
      objects: List[Namespaced[MemberSyntax.ObjectDeclarationSyntax]],
      classes: List[Namespaced[MemberSyntax.ClassDeclarationSyntax]],
      enums: List[Namespaced[MemberSyntax.EnumDeclarationSyntax]],
      traits: List[Namespaced[MemberSyntax.TraitDeclarationSyntax]],
      givens: List[Namespaced[MemberSyntax.GivenDeclarationSyntax]],
      fields: List[MemberSyntax.VariableDeclaration],
      functions: List[MemberSyntax.FunctionDeclarationSyntax],
      globalStatements: List[MemberSyntax.GlobalStatementSyntax]
  ): Members = {
    members match {
      case List.Nil =>
        trees match {
          case List.Nil =>
            Members(
              objects,
              classes,
              functions,
              enums,
              traits,
              givens,
              fields,
              globalStatements
            )
          case List.Cons(tree, tail) =>
            _splitMembers(
              tree.root.namespaceDeclaration match {
                case Option.None        => List.Nil
                case Option.Some(value) => nameToNamespace(value.name, List.Nil)
              },
              tail,
              tree.root.members,
              objects,
              classes,
              enums,
              traits,
              givens,
              fields,
              functions,
              globalStatements
            )
        }

      case List.Cons(member, tail) =>
        // TODO: rework this so its more performant
        // complete remaining members
        val rest = _splitMembers(
          ns,
          trees,
          tail,
          objects,
          classes,
          enums,
          traits,
          givens,
          fields,
          functions,
          globalStatements
        )

        addMember(ns, member, rest)
    }
  }

  def addMember(
      ns: List[string],
      member: MemberSyntax,
      rest: Members
  ): Members = {
    member match {
      case member: MemberSyntax.ObjectDeclarationSyntax =>
        Members(
          List.Cons(Namespaced(ns, member), rest.objects),
          rest.classes,
          rest.functions,
          rest.enums,
          rest.traits,
          rest.givens,
          rest.fields,
          rest.globalStatements
        )
      case member: MemberSyntax.ClassDeclarationSyntax =>
        Members(
          rest.objects,
          List.Cons(Namespaced(ns, member), rest.classes),
          rest.functions,
          rest.enums,
          rest.traits,
          rest.givens,
          rest.fields,
          rest.globalStatements
        )
      case member: MemberSyntax.FunctionDeclarationSyntax =>
        Members(
          rest.objects,
          rest.classes,
          List.Cons(member, rest.functions),
          rest.enums,
          rest.traits,
          rest.givens,
          rest.fields,
          rest.globalStatements
        )
      case member: MemberSyntax.EnumDeclarationSyntax =>
        Members(
          rest.objects,
          rest.classes,
          rest.functions,
          List.Cons(Namespaced(ns, member), rest.enums),
          rest.traits,
          rest.givens,
          rest.fields,
          rest.globalStatements
        )
      case member: MemberSyntax.TraitDeclarationSyntax =>
        Members(
          rest.objects,
          rest.classes,
          rest.functions,
          rest.enums,
          List.Cons(Namespaced(ns, member), rest.traits),
          rest.givens,
          rest.fields,
          rest.globalStatements
        )
      case member: MemberSyntax.GivenDeclarationSyntax =>
        Members(
          rest.objects,
          rest.classes,
          rest.functions,
          rest.enums,
          rest.traits,
          List.Cons(Namespaced(ns, member), rest.givens),
          rest.fields,
          rest.globalStatements
        )
      case member: MemberSyntax.GlobalStatementSyntax =>
        Members(
          rest.objects,
          rest.classes,
          rest.functions,
          rest.enums,
          rest.traits,
          rest.givens,
          rest.fields,
          List.Cons(member, rest.globalStatements)
        )
      case variable: MemberSyntax.VariableDeclaration =>
        // convert variable declaration to a global statement
        val statement = new MemberSyntax.GlobalStatementSyntax(
          StatementSyntax.ExpressionStatement(
            Expression.Assignment(
              Expression.IdentifierName(
                SimpleNameSyntax.IdentifierNameSyntax(variable.identifier)
              ),
              variable.equalToken,
              variable.expression
            )
          )
        )

        // merge the variable into the other members
        Members(
          rest.objects,
          rest.classes,
          rest.functions,
          rest.enums,
          rest.traits,
          rest.givens,
          List.Cons(variable, rest.fields),
          List.Cons(statement, rest.globalStatements)
        )
    }
  }

  /** Detects multiple source files with top level statements.
    *
    *   - if there are multiple source files with top level statements or top
    *     level functions report an error
    */
  def detectMultipleSourceFilesWithTopLevelStatements(
      functions: List[MemberSyntax.FunctionDeclarationSyntax],
      globalStatements: List[MemberSyntax.GlobalStatementSyntax]
  ): unit = {
    functions match {
      case List.Nil =>
        globalStatements match {
          case List.Nil => ()
          case List.Cons(head, tail) =>
            _detectMultipleSourceFilesWithTopLevelStatements(
              AstUtils.locationOfMember(head),
              functions,
              tail
            )
        }
      case List.Cons(head, tail) =>
        _detectMultipleSourceFilesWithTopLevelStatements(
          AstUtils.locationOfMember(head),
          tail,
          globalStatements
        )
    }
  }

  def _detectMultipleSourceFilesWithTopLevelStatements(
      firstLocation: TextLocation,
      functions: List[MemberSyntax.FunctionDeclarationSyntax],
      globalStatements: List[MemberSyntax.GlobalStatementSyntax]
  ): unit = {
    functions match {
      case List.Nil =>
        globalStatements match {
          case List.Nil => ()
          case List.Cons(head, tail) =>
            val memberLocation = AstUtils.locationOfMember(head)
            if (memberLocation.sourceFile != firstLocation.sourceFile) {
              diagnosticBag.reportTopLevelStatementsInMultipleFiles(
                firstLocation,
                memberLocation
              )
            }
            _detectMultipleSourceFilesWithTopLevelStatements(
              firstLocation,
              functions,
              tail
            )
        }
      case List.Cons(head, tail) =>
        val memberLocation = AstUtils.locationOfMember(head)
        if (memberLocation.sourceFile != firstLocation.sourceFile) {
          diagnosticBag.reportTopLevelStatementsInMultipleFiles(
            firstLocation,
            memberLocation
          )
        }
        _detectMultipleSourceFilesWithTopLevelStatements(
          firstLocation,
          tail,
          globalStatements
        )
    }
  }
}
