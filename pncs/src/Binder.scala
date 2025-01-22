import panther.*

case class Members(
    objects: List[Namespaced[MemberSyntax.ObjectDeclarationSyntax]],
    classes: List[Namespaced[MemberSyntax.ClassDeclarationSyntax]],
    functions: List[MemberSyntax.FunctionDeclarationSyntax],
    enums: List[Namespaced[MemberSyntax.EnumDeclarationSyntax]],
    fields: List[MemberSyntax.VariableDeclaration],

    // top level variable declarations are converted to top level assignments so that we can maintain the order
    // within the set of statements. the statements are then moved to the relevant constructor
    globalStatements: List[MemberSyntax.GlobalStatementSyntax]
)

case class Entry(
    program: Option[MemberSyntax.ObjectDeclarationSyntax],
    main: Option[MemberSyntax.FunctionDeclarationSyntax],
    extraStatements: List[MemberSyntax.GlobalStatementSyntax],
    members: Members
)

case class Namespaced[A](ns: List[string], value: A)

enum BindingMember {
  case Method(value: MemberSyntax.FunctionDeclarationSyntax)
  case Field(value: MemberSyntax.VariableDeclaration)
  case Parameter(value: ParameterSyntax)
}

enum TypingMember {
  case Method(genericParameters: List[Type], parameters: List[BoundParameter], returnType: Option[Type], expression: Option[Expression], scope: Scope)
  case Field(options: FieldOptions, scope: Scope)
}

enum FieldOptions {
  case TypeAndExpression(fieldType: Type, expression: Expression)
  case TypeOnly(fieldType: Type)
  case ExpressionOnly(expression: Expression)
}

case class ConstructorParams(genericTypeParameters: List[Type], parameters: List[ParameterSyntax])

class Binder(
    trees: List[SyntaxTree],
    rootSymbol: Symbol,
    diagnosticBag: DiagnosticBag
) {

  var symbolTypes: Dictionary[int, SymbolLinks] = DictionaryModule.empty()
  var nextSymbolId = 0

  def setSymbolType(symbol: Symbol, typ: Type, base: Option[Symbol]): Type = {
    val id = getSymbolId(symbol)
    symbolTypes = symbolTypes.put(id, SymbolLinks(typ, base))
    typ
  }

  def getSymbolId(symbol: Symbol): int = {
    if (symbol._id == -1) {
      symbol._id = nextSymbolId
      nextSymbolId = nextSymbolId + 1
    }
    symbol._id
  }

  def getSymbolType(symbol: Symbol): Option[Type] =
    symbolTypes.get(symbol._id) match {
      case Option.None => Option.None
      case Option.Some(value) => Some(value.typ)
    }

  val anyType = new Type.Named(List.Nil, "any", List.Nil)
  val stringType = new Type.Named(List.Nil, "string", List.Nil)
  val intType = new Type.Named(List.Nil, "int", List.Nil)
  val charType = new Type.Named(List.Nil, "char", List.Nil)
  val boolType = new Type.Named(List.Nil, "bool", List.Nil)
  val unitType = new Type.Named(List.Nil, "unit", List.Nil)
  val neverType = new Type.Named(List.Nil, "never", List.Nil)
  val noneType = new Type.Named(List.Nil, "Option", List.Cons(Type.Never, List.Nil))

  val pantherNamespace = rootSymbol // TODO: move these to the panther namespace .enter("panther")

  val anySymbol =
    pantherNamespace.defineClass("any", TextLocationFactory.empty())
  symbolTypes = symbolTypes.put(getSymbolId(anySymbol), SymbolLinks(anyType, None))

  val intSymbol =
    pantherNamespace.defineClass("int", TextLocationFactory.empty())
  setSymbolType(intSymbol, intType, Some(anySymbol))

  val stringSymbol = pantherNamespace.defineClass(
    "string",
    TextLocationFactory.empty()
  )
  setSymbolType(stringSymbol, stringType, Some(anySymbol))

  val boolSymbol = pantherNamespace.defineClass(
    "bool",
    TextLocationFactory.empty()
  )
  setSymbolType(boolSymbol, boolType, Some(anySymbol))

  val charSymbol = pantherNamespace.defineClass(
    "char",
    TextLocationFactory.empty()
  )
  setSymbolType(charSymbol, charType, Some(anySymbol))

  val unitSymbol = pantherNamespace.defineClass(
    "unit",
    TextLocationFactory.empty()
  )
  setSymbolType(unitSymbol, unitType, Some(anySymbol))

  val arraySymbol = pantherNamespace.defineClass(
    "Array",
    TextLocationFactory.empty()
  )

  setSymbolType(
    arraySymbol.defineMethod(".ctor", TextLocationFactory.empty()),
    Type.Function(List.Nil, List.Cons(BoundParameter("size", intType), List.Nil), unitType),
    Option.None
  )

  setSymbolType(
    arraySymbol,
    Type.Named(List.Nil, "Array",
      List.Cons(
        setSymbolType(
          arraySymbol.defineTypeParameter(
            "T",
            TextLocationFactory.empty(),
            Variance.Invariant
          ),
          Type.Variable("T", Variance.Invariant, None),
          None
        ),
        List.Nil
      )
    ),
    Some(anySymbol)
  )

  //  string.length: int
  setSymbolType(
    stringSymbol.defineField(
      "length",
      TextLocationFactory.empty()
    ),
    intType,
    None
  )

  val predef = pantherNamespace.defineObject(
    "predef",
    TextLocationFactory.empty()
  )

  // println(message: string): unit
  val printlnSymbol = predef.defineMethod(
    "println",
    TextLocationFactory.empty()
  )
  setSymbolType(printlnSymbol, Type.Function(List.Nil, List.Cons(BoundParameter("message", stringType), List.Nil), unitType), None)
  setSymbolType(
    printlnSymbol.defineParameter(
      "message",
      TextLocationFactory.empty()
    ),
    stringType,
    None
  )

  // print(message: string): unit
  val printSymbol = predef.defineMethod(
    "print",
    TextLocationFactory.empty()
  )
  setSymbolType(printSymbol, Type.Function(List.Nil, List.Cons(BoundParameter("message", stringType), List.Nil), unitType), None)

  setSymbolType(
    printSymbol.defineParameter(
      "message",
      TextLocationFactory.empty()
    ),
    stringType,
    None
  )

  /**
   * These are the methods and fields that need to be bound/symbolized
   */
  var membersToBind: Dictionary[Symbol, List[BindingMember]] = DictionaryModule.empty()

  /**
   * statementsToBind represent the statements that occur in the body of a class,
   * these statements will be converted into a constructor for the class/enum/object
   */
  var statementsToBind
  : Dictionary[Symbol, List[MemberSyntax.GlobalStatementSyntax]] =
    DictionaryModule.empty()

  /** constructors to build */
  var ctorsToBind: Dictionary[Symbol, ConstructorParams] = DictionaryModule.empty()

  /** functionBodies are the bound expressions for all methods and constructors */
  var functionBodies : Dictionary[Symbol, BoundExpression] = DictionaryModule.empty()

  /** map of member symbols to their untyped declarations */
  var membersToType : Dictionary[Symbol, TypingMember] = DictionaryModule.empty()

  val classifier = new ConversionClassifier(this)
  val exprBinder: ExprBinder = new ExprBinder(rootSymbol, this, classifier,  diagnosticBag)

  def bind(): BoundAssembly = {
    // take ast, extract top level statements
    val members = splitMembersInTrees(trees)

    // make sure there are not more than one source file with top level statements
    detectMultipleSourceFilesWithTopLevelStatements(
      members.functions,
      members.globalStatements
    )

    // find Program class
    val program = getProgramObject(members.objects)

    // find main method
    val mainAsGlobal = getMainMethod(members.functions)
    val mainFromProgram = program match {
      case Option.Some(p) =>
        val members =
          splitMembers(p.ns, p.value.template.members)
        getMainMethod(members.functions)
      case Option.None => Option.None
    }
    val mainMethod = Tuple2(mainAsGlobal, mainFromProgram) match {
      case Tuple2(Option.Some(global), Option.None) => Option.Some(global)
      case Tuple2(Option.None, Option.Some(program)) => Option.Some(program)

      case Tuple2(Option.Some(global), Option.Some(program)) =>
        // report error if we have two mains
        diagnosticBag.reportMultipleEntryPoints(
          AstUtils.locationOfMember(global),
          AstUtils.locationOfMember(program)
        )
        Option.Some(global)
      case Tuple2(Option.None, Option.None) => Option.None
    }

    val objects = removeProgram(members.objects)
    val functions = removeMain(members.functions)
    val enums = members.enums

    // start binding/typing analysis
    val rootScope = Scope(rootSymbol, List.Nil)

    // bind all objects and classes first
    bindClassesObjectAndEnums(members.classes, objects, enums, rootScope)

    //    println("Binding complete")
//    val counts = countMembersToBind(membersToBind.list, 0, 0)
//    println(string(counts._1 + counts._2) + " members to bind")
    //    printStatementsToBind(statementsToBind.list)

    // bind all members function & field types with type annotations
    membersToType = bindMembers(membersToBind.list, 1, DictionaryModule.empty())

//    panic("need to create ctors for classes")

    // TODO: this method still needs to register the field assignments as ctor statements
    bindConstructorSignatures(ctorsToBind.list)

//    panic(string(membersToType.length) + " members to type")

    // then bind all functions & fields without type annotations
    bindTypingMembers()

    // bind all `statementsToBind` as constructor bodies


//    panic("the type above probably should include the symbols we started to type, as well as all the parameters for methods")

    // then bind all function bodies

    // bind entry point
    //    val main =
    //      bindEntry(program, mainMethod, functions, members.globalStatements)


    BoundAssembly(List.Nil, diagnosticBag.diagnostics, Option.None)
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
            setSymbolType(ctorSymbol, Type.Function(typParams, params, unitType), None)
        }


        bindConstructorSignatures(tail)
    }
  }

  def bindTypingMembers(): unit = {
    membersToType.list  match {
      case List.Nil => ()
      case List.Cons(KeyValue(symbol, member), tail) =>
        bindOneTypingMember(symbol, member)
        bindTypingMembers()
    }
  }

  def bindOneTypingMember(symbol: Symbol, member: TypingMember): Type = {
    membersToType = membersToType.remove(symbol)
    member match {
      case TypingMember.Method(genTypeParams, parameters, returnType, expression, scope) =>
        bindMethodBody(symbol, genTypeParams, parameters, returnType, expression, scope)
      case TypingMember.Field(options, scope) =>
        bindFieldExpression(symbol, options, scope)
    }
  }

  def bindFieldExpression(symbol: Symbol, options: FieldOptions, scope: Scope): Type = {
    options match {
      case FieldOptions.TypeAndExpression(fieldType, expression) =>
        val expr = exprBinder.bindConversionExpr(expression,fieldType, scope)
        setSymbolType(symbol, fieldType, None)
      case FieldOptions.TypeOnly(fieldType) =>
        setSymbolType(symbol, fieldType, None)
      case FieldOptions.ExpressionOnly(expression) =>
        val expr = exprBinder.bind(expression, scope)
        // TODO: this expression needs to be added to the symbol's constructor body
        val returnType = getType(expr)
        setSymbolType(symbol, returnType, None)
    }
  }

  def bindMethodBody(symbol: Symbol, args: List[Type], parameters: List[BoundParameter], returnType: Option[Type], expression: Option[Expression], methodScope: Scope): Type = {

    // if returnType is None, then we need to infer the type and set the methods type
    val expr = expression match {
      case Option.None => None
      case Option.Some(expression) =>

        val boundExpr = returnType match {
          case Option.None => exprBinder.bind(expression, methodScope)
          case Option.Some(toType) =>
            exprBinder.bindConversionExpr(expression, toType, methodScope)
        }

        functionBodies = functionBodies.put(symbol, boundExpr)
        Some(boundExpr)
    }

    returnType match {
      case Option.None =>
        expr match {
          case Option.None =>
            // TODO: it is an error if returnType and expression are None at the moment
            panic("returnType and expression are None")

          case Option.Some(value) =>
            val ret = getType(value)
            setSymbolType(symbol, Type.Function(args, parameters, ret), None)
        }
      case Option.Some(value) =>
        setSymbolType(symbol, Type.Function(args, parameters, value), None)
    }
  }

  def getTypeSymbol(typ: Type): Option[Symbol] = {
    typ match {
      case Type.Named(ns, name, _) =>
        // todo: handle type args being returned
        rootSymbol.findSymbol(ns, name)
      case _ => Option.None
    }
  }

  def getType(expr: BoundExpression): Type = {
    expr match {
      case BoundExpression.Error => Type.Error
      case _: BoundExpression.IntLiteral => intType
      case _: BoundExpression.StringLiteral => stringType
      case _: BoundExpression.BooleanLiteral => boolType
      case _: BoundExpression.CharacterLiteral => charType
      case _: BoundExpression.ForExpression => unitType
      case _: BoundExpression.UnitExpression => unitType
      case _: BoundExpression.WhileExpression => unitType

      case expr: BoundExpression.BinaryExpression => expr.resultType
      case expr: BoundExpression.Block => getType(expr.expression)
      case expr: BoundExpression.CallExpression => expr.resultType
      case expr: BoundExpression.CastExpression => expr.targetType
      case expr: BoundExpression.IndexExpression => expr.resultType
      case expr: BoundExpression.IfExpression => expr.resultType
      case expr: BoundExpression.NewExpression => expr.resultType
      case expr: BoundExpression.UnaryExpression => expr.resultType
      case expr: BoundExpression.Variable =>
        val symbol = expr.symbol
        getSymbolType(symbol) match {
          case Option.None =>
            membersToType.get(symbol) match {
              case Option.None =>
                // no type has been detected yet so lets bind the entry and then try again
                println("Unknown type for symbol " + symbol.kind)
                diagnosticBag.reportBugUnknownType(expr.location, symbol.name)
                Type.Error
              case Option.Some(value) =>
                bindOneTypingMember(symbol, value)
            }
          case Option.Some(value) => value
        }

      //      case _ =>
      //        panic("getType not implemented for " + expr)
    }
  }

  def bindMembers(
                   list: List[KeyValue[Symbol, List[BindingMember]]],
                   phase: int,
                   acc: Dictionary[Symbol, TypingMember]
                 ): Dictionary[Symbol, TypingMember] = {
    list match {
      case List.Nil => acc
      case List.Cons(KeyValue(symbol, members), tail) =>
        // TODO: need to make scope that incorporates imports from the SyntaxTree
        //  that the symbol was defined in
        val scope = Scope(symbol, List.Nil)
        bindMembers(tail, phase, bindSymbolMembers(members,phase, acc, scope))
    }
  }

  /**
   * Create symbols for all the members of a class, object, or enum
   *
   * If the member has a type annotation, set the type, otherwise add
   * to the members to type(acc)
   * @param list
   * @param phase
   * @param acc
   * @param scope
   * @return
   */
  def bindSymbolMembers(list: List[BindingMember], phase: int, acc: Dictionary[Symbol, TypingMember], scope: Scope): Dictionary[Symbol, TypingMember] = {
    list match {
      case List.Nil => acc
      case List.Cons(head, tail) =>
        val newAcc = head match {
          case BindingMember.Method(value) => bindMethod(value, acc, scope)
          case BindingMember.Field(value) => bindField(value, acc, scope)
          case BindingMember.Parameter(value) =>
            bindParameterAsField(value, acc, scope)
        }

        bindSymbolMembers(tail, phase, newAcc, scope)
    }
  }

  /**
   * Bind a field
   *
   * @param value
   * @param scope
   * @return a TypingMember if the field was not successfully typed
   */
  def bindField(value: MemberSyntax.VariableDeclaration, acc: Dictionary[Symbol, TypingMember], scope: Scope): Dictionary[Symbol, TypingMember] = {
    scope.defineField(value.identifier.text, value.identifier.location) match {
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
            setSymbolType(symbol, returnType, None)
            FieldOptions.TypeAndExpression(returnType, value.expression)
        }
        acc.put(symbol, TypingMember.Field(options, scope))
    }
  }


  def bindParameterAsField(value: ParameterSyntax, acc: Dictionary[Symbol, TypingMember], scope: Scope): Dictionary[Symbol, TypingMember] = {
    scope.defineField(value.identifier.text, value.identifier.location) match {
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
        setSymbolType(symbol, returnType, None)
        acc.put(symbol, TypingMember.Field(FieldOptions.TypeOnly(returnType), scope))
    }
  }


  /**
   * Bind a method
   *
   * @param value the method to bind
   * @param scope
   * @return a TypingMember if the method was not successfully typed
   */
  def bindMethod(value: MemberSyntax.FunctionDeclarationSyntax, acc: Dictionary[Symbol, TypingMember], scope: Scope): Dictionary[Symbol, TypingMember] = {
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

        val parameters = bindParameters(value.parameters, methodScope)
        val expr = value.body match {
          case Option.None => None
          case Option.Some(body) => Some(body.expression)
        }
        val returnType = value.typeAnnotation match {
          case Option.None => None // infer type later
          case Option.Some(typeAnnotation) =>
            val returnType = bindTypeName(typeAnnotation.typ, methodScope)
            setSymbolType(symbol, Type.Function(genTypeParams, parameters, returnType), None)
            Some(returnType)
        }

        acc.put(symbol, TypingMember.Method(genTypeParams, parameters, returnType, expr, methodScope))
    }
  }

  def bindParameters(parameters: List[ParameterSyntax], scope: Scope): List[BoundParameter] = {
    parameters match {
      case List.Nil => List.Nil
      case List.Cons(head, tail) =>
        val typ = bindTypeName(head.typeAnnotation.typ, scope)
        scope.defineParameter(head.identifier.text, head.identifier.location) match {
          case Either.Left(location) =>
            diagnosticBag.reportDuplicateDefinition(
              head.identifier.text,
              location,
              head.identifier.location
            )
            bindParameters(tail, scope)
          case Either.Right(symbol) =>
            setSymbolType(symbol, typ, None)
            List.Cons(BoundParameter(head.identifier.text, typ), bindParameters(tail, scope))
        }
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

  def bindTypeArgumentList(arguments: Array[TypeArgumentItemSyntax], scope: Scope): List[Type] =
    bindTypeArguments(ListModule.fromArray(arguments), scope)

  def bindTypeArguments(arguments: List[TypeArgumentItemSyntax], scope: Scope): List[Type] = {
    arguments match {
      case List.Nil => List.Nil
      case List.Cons(head, tail) =>
        List.Cons(bindTypeName(head.name, scope), bindTypeArguments(tail, scope))
    }
  }

  def bindTypeSimpleName(name: SimpleNameSyntax, top: bool, scope: Scope): Type = {
    name match {
      case SimpleNameSyntax.GenericNameSyntax(identifier, typeArgumentlist) =>
        scope.lookup(identifier.text) match {
          case Option.None =>
            diagnosticBag.reportTypeNotDefined(identifier.location, identifier.text)
            Type.Error
          case Option.Some(symbol) =>
            // TODO: need to add diagnostic for verifying that there are the correct
            //  number of type arguments on `symbol`
            val typeArguments = bindTypeArgumentList(typeArgumentlist.arguments, scope)
            new Type.Named(symbol.ns(), symbol.name, typeArguments)
        }
      case SimpleNameSyntax.IdentifierNameSyntax(identifier) =>
        if (top && identifier.text == "any") {
          Type.Any
        } else if (top && identifier.text == "never") {
          Type.Never
        } else {
          scope.lookup(identifier.text) match {
            case Option.None =>
              diagnosticBag.reportTypeNotDefined(identifier.location, identifier.text)
              Type.Error
            case Option.Some(value) =>
              new Type.Named(value.ns(), value.name, List.Nil)
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
                          list: List[KeyValue[Symbol, List[BindingMember]]], fields: int, methods: int
                        ): Tuple2[int, int] = {
    list match {
      case List.Nil => Tuple2(fields, methods)
      case List.Cons(KeyValue(symbol, members), tail) =>
        val methodsAndFields = countMembers(members, 0, 0)
        countMembersToBind(tail, fields + methodsAndFields._2, methods + methodsAndFields._1)
    }
  }

  def countMembers(list: List[BindingMember], methods: int, fields: int): Tuple2[int, int] = {
    list match {
      case List.Nil => Tuple2(methods, fields)
      case List.Cons(BindingMember.Method(_), tail) => countMembers(tail, methods + 1, fields)
      case List.Cons(BindingMember.Field(_), tail) => countMembers(tail, methods, fields + 1)
      case List.Cons(BindingMember.Parameter(_), tail) => countMembers(tail, methods, fields + 1)
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
                                 scope: Scope
                               ): unit = {
    bindObjects(objects, scope)
    bindClasses(classes, scope)
    bindEnums(enums, scope)
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
    val enumSymbol = scope.defineEnum(
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

        setSymbolType(symbol, new Type.Named(symbol.ns(), symbol.name, genericParameters), None)

        val cases = ListModule.fromArray(head.value.cases)
        bindEnumCases(cases, enumScope)

        val members = splitMembers(List.Nil, head.value.members)
        bindClassesObjectAndEnums(
          members.classes,
          members.objects,
          members.enums,
          enumScope
        )

        addMembersToBind(symbol, members.functions, members.fields, List.Nil)
        addStatementsToBind(symbol, members.globalStatements)
    }
  }

  def bindEnumCases(cases: List[EnumCaseSyntax], scope: Scope): unit = {
    cases match {
      case List.Nil => ()
      case List.Cons(enumCase, tail) =>
        val name = enumCase.identifier.text
        val location = enumCase.identifier.location
        scope.defineClass(name, location) match {
          case Either.Left(location) =>
            diagnosticBag.reportDuplicateDefinition(name, location, location)
          case Either.Right(caseSymbol) =>
            val parent = scope.current
            setSymbolType(caseSymbol, new Type.Named(caseSymbol.ns(), caseSymbol.name, List.Nil), Some(parent))
            enumCase.parameters match {
              case Option.None =>
              case Option.Some(value) =>
                addMembersToBind(caseSymbol, List.Nil, List.Nil, value.parameters)
                // TODO: we need base type here to get the generic type parameters
                ctorsToBind = ctorsToBind.put(caseSymbol, ConstructorParams(List.Nil, value.parameters))
            }
        }
        bindEnumCases(tail, scope)
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
            setSymbolType(symbol, new Type.Named(symbol.ns(), symbol.name, List.Nil), None)

            ctorsToBind = ctorsToBind.put(symbol, ConstructorParams(List.Nil, head.value.parameters))
          case Option.Some(value) =>
            val args = bindGenericTypeParameters(value.parameters.items, scope.enterSymbol(symbol))
            setSymbolType(symbol, new Type.Named(symbol.ns(), symbol.name, args), None)

            ctorsToBind = ctorsToBind.put(symbol, ConstructorParams(args, head.value.parameters))
        }

        head.value.template match {
          case Option.None => ()
          case Option.Some(template) =>
            val members = splitMembers(List.Nil, template.members)
            bindClassesObjectAndEnums(
              members.classes,
              members.objects,
              members.enums,
              scope.enterSymbol(symbol)
            )
            addMembersToBind(symbol, members.functions, members.fields, head.value.parameters)
            addStatementsToBind(symbol, members.globalStatements)
        }
    }
  }

  def bindGenericTypeParameters(value: List[GenericParameterSyntax], scope: Scope): List[Type] = {
    value match {
      case List.Nil => List.Nil
      case List.Cons(GenericParameterSyntax(varianceToken, identifier, _), tail) =>
        val variance = varianceToken match {
          case Option.None => Variance.Invariant
          case Option.Some(value) =>
            value.text match {
              case "+" => Variance.Covariant
              case "-" => Variance.Contravariant
              case "in" => Variance.Covariant
              case "out" => Variance.Contravariant
              case x => panic("Invalid variance token: " + x)
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
            diagnosticBag.reportDuplicateDefinition(name, value, identifier.location)
            bindGenericTypeParameters(tail, scope)
          case Either.Right(value) =>
            setSymbolType(value, Type.Variable(name, variance, None), None)
            List.Cons(Type.Variable(name, variance, None), bindGenericTypeParameters(tail, scope))
        }
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
          scope.enterSymbol(symbol)
        )
        setSymbolType(symbol, new Type.Named(symbol.ns(), symbol.name, List.Nil), None)
        addMembersToBind(symbol, members.functions, members.fields, List.Nil)
        addStatementsToBind(symbol, members.globalStatements)
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
        functionsToMembers(parametersToMembers(List.Nil, parameters), functions),
        fields
      )

    if (members.length > 0) {
      membersToBind = membersToBind.put(symbol, members)
    }
  }

  def parametersToMembers(members: List[BindingMember], parameters: List[ParameterSyntax]): List[BindingMember] = {
    parameters match {
      case List.Nil => members
      case List.Cons(head, tail) =>
        parametersToMembers(List.Cons(BindingMember.Parameter(head), members), tail)
    }
  }

  def functionsToMembers(members: List[BindingMember], functions: List[MemberSyntax.FunctionDeclarationSyntax]): List[BindingMember] = {
    functions match {
      case List.Nil => members
      case List.Cons(head, tail) =>
        functionsToMembers(List.Cons(BindingMember.Method(head), members), tail)
    }
  }

  def variablesToMembers(members: List[BindingMember], variables: List[MemberSyntax.VariableDeclaration]): List[BindingMember] = {
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
    if (statements.length > 0) {
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

  def bindEntry(
                 program: Option[Namespaced[MemberSyntax.ObjectDeclarationSyntax]],
                 main: Option[MemberSyntax.FunctionDeclarationSyntax],
                 programMethods: List[MemberSyntax.FunctionDeclarationSyntax],
                 statements: List[MemberSyntax.GlobalStatementSyntax]
               ): BoundEntry = {

    // if program class does not exist, create it
    // if main method does not exist, create it

    // move all top level functions to Program class

    // move all top level statements to main method and bind main
    ???
  }

  def getMainMethod(
                     functions: List[MemberSyntax.FunctionDeclarationSyntax]
                   ): Option[MemberSyntax.FunctionDeclarationSyntax] = {
    functions match {
      case List.Nil => Option.None
      case List.Cons(head, tail) =>
        if (head.identifier.text == "main") {
          Option.Some(head)
        } else {
          getMainMethod(tail)
        }
    }
  }

  def getProgramObject(
                        objects: List[Namespaced[MemberSyntax.ObjectDeclarationSyntax]]
                      ): Option[Namespaced[MemberSyntax.ObjectDeclarationSyntax]] = {
    objects match {
      case List.Nil => Option.None
      case List.Cons(head, tail) =>
        if (head.value.identifier.text == "Program") {
          Option.Some(head)
        } else {
          getProgramObject(tail)
        }
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
              fields,
              globalStatements
            )
          case List.Cons(tree, tail) =>
            _splitMembers(
              tree.root.namespaceDeclaration match {
                case Option.None => List.Nil
                case Option.Some(value) => nameToNamespace(value.name, List.Nil)
              },
              tail,
              tree.root.members,
              objects,
              classes,
              enums,
              fields,
              functions,
              globalStatements
            )
        }

      case List.Cons(member, tail) =>
        member match {
          case member: MemberSyntax.ObjectDeclarationSyntax =>
            _splitMembers(
              ns,
              trees,
              tail,
              List.Cons(Namespaced(ns, member), objects),
              classes,
              enums,
              fields,
              functions,
              globalStatements
            )
          case member: MemberSyntax.ClassDeclarationSyntax =>
            _splitMembers(
              ns,
              trees,
              tail,
              objects,
              List.Cons(Namespaced(ns, member), classes),
              enums,
              fields,
              functions,
              globalStatements
            )
          case member: MemberSyntax.FunctionDeclarationSyntax =>
            _splitMembers(
              ns,
              trees,
              tail,
              objects,
              classes,
              enums,
              fields,
              List.Cons(member, functions),
              globalStatements
            )
          case member: MemberSyntax.EnumDeclarationSyntax =>
            _splitMembers(
              ns,
              trees,
              tail,
              objects,
              classes,
              List.Cons(Namespaced(ns, member), enums),
              fields,
              functions,
              globalStatements
            )
          case member: MemberSyntax.GlobalStatementSyntax =>
            _splitMembers(
              ns,
              trees,
              tail,
              objects,
              classes,
              enums,
              fields,
              functions,
              List.Cons(member, globalStatements)
            )
          case variable: MemberSyntax.VariableDeclaration =>
            val statement = new MemberSyntax.GlobalStatementSyntax(
              StatementSyntax.ExpressionStatement(
                Expression.AssignmentExpression(
                  Expression.IdentifierName(
                    SimpleNameSyntax.IdentifierNameSyntax(variable.identifier)
                  ),
                  variable.equalToken,
                  variable.expression
                )
              )
            )
            _splitMembers(
              ns,
              trees,
              tail,
              objects,
              classes,
              enums,
              List.Cons(variable, fields),
              functions,
              List.Cons(statement, globalStatements)
            )
        }
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
