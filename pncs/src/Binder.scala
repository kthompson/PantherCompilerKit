import panther._

case class Binder(diagnostics: DiagnosticBag, root: Symbol) {

  var ns = "" // namespace
  var _parent = root

  def bind(trees: Array[SyntaxTree]): BoundTree = {
    val bag = new DiagnosticBag()
    for (i <- 0 to (trees.length - 1)) {
      bag.addDiagnostics(trees(i).diagnostics)
    }

    var defns = BoundDefinitions.Empty
    for (x <- 0 to (trees.length - 1)) {
      defns = mergeDefinitions(defns, bindCompilationUnit(trees(x).root))
    }

    BoundTree(root, bag.diagnostics, defns)
  }

  def bindMethod(parent: Symbol, node: MemberSyntax.FunctionDeclarationSyntax): BoundDefinition.Method = {
    // TODO: need to handle non-static methods
    val functionSymbol = declareSymbol(SymbolKind.Method, SymbolFlags.Static, Declaration.Method(node.identifier.text, node.identifier.location, node), parent.members, parent)
    val params = bindParameters(node.parameters, functionSymbol.members)
    val body = node.body match {
      case Some(value) => Some(value.expression)
      case None => None
    }
    new BoundDefinition.Method(functionSymbol, node, params, body)
  }

  def bindFieldFromParam(parent: Symbol, node: ParameterSyntax): BoundField = {
    val fieldSymbol = declareSymbol(
      SymbolKind.Field,
      parent.flags & SymbolFlags.Static,
      Declaration.FieldFromParameter(node.identifier.text, node.identifier.location, node),
      parent.members,
      parent
    )

    BoundField(fieldSymbol, Some(node.typeAnnotation), None)
  }

  def bindCompilationUnit(compilationUnit: CompilationUnitSyntax): BoundDefinitions = {
    // bind_namespace_declarations(compilationUnit.namespaceDeclaration)
    // bind_using_declarations(compilationUnit.usings)
    bindMembers(compilationUnit.members, root.members)
  }

  def bindCases(nodes: Array[EnumCaseSyntax], scope: Scope): Array[BoundEnumCase] = {
    val cases = new Array[BoundEnumCase](nodes.length)
    for (x <- 0 to (nodes.length - 1)) {
      cases(x) = bindCase(nodes(x), scope)
    }
    cases
  }

  def bindCase(node: EnumCaseSyntax, scope: Scope): BoundEnumCase = {
    val saveParent = _parent
    val symbol = declareSymbol(SymbolKind.Class, SymbolFlags.None, Declaration.ClassFromEnumCase(node.identifier.text, node.identifier.location, node), scope, _parent)

    // todo bind fields
    val params = bindEnumConstructor(node, symbol.members)

    _parent = saveParent

    new BoundEnumCase(symbol, params)
  }

  def bindEnumConstructor(node: EnumCaseSyntax, scope: Scope): Array[BoundParameter] = {
    node.parameters match {
      case Some(EnumCaseParametersSyntax(_, parameters, _)) =>
        val saveParent = _parent
        val symbol = declareSymbol(SymbolKind.Constructor, SymbolFlags.None, Declaration.Constructor(node.identifier.text, node.identifier.location, parameters), scope, _parent)
        _parent = symbol

        val params = bindParameters(parameters, symbol.members)
        _parent = saveParent
        params
      case None =>
        new Array[BoundParameter](0)
    }
  }

  def bindMembers(nodes: Array[MemberSyntax], scope: Scope): BoundDefinitions = {
    var defns = BoundDefinitions.Empty
    for (x <- 0 to (nodes.length - 1)) {
      defns = BoundDefinitions.Cons(bindMember(nodes(x), scope), defns)
    }
    defns
  }
  
  def mergeDefinitions(a: BoundDefinitions, b: BoundDefinitions): BoundDefinitions = {
    a match {
      case BoundDefinitions.Empty => b
      case BoundDefinitions.Cons(definition, tail) => 
        mergeDefinitions(tail, BoundDefinitions.Cons(definition, b))
    }
  }

  def bindMember(member: MemberSyntax, scope: Scope): BoundDefinition = {
    member match {
      case value: MemberSyntax.ObjectDeclarationSyntax => bindObjectDeclaration(value, scope)
      case value: MemberSyntax.ClassDeclarationSyntax => bindClassDeclaration(value, scope)
      case value: MemberSyntax.FunctionDeclarationSyntax => bindFunctionDeclaration(value, scope)
      case value: MemberSyntax.EnumDeclarationSyntax => bindEnumDeclaration(value, scope)
      case value: MemberSyntax.VariableDeclaration => bindVariableDeclaration(value, scope)
      case value: MemberSyntax.GlobalStatementSyntax => bindGlobalStatement(value, scope)
    }
  }

  def bindGlobalStatement(node: MemberSyntax.GlobalStatementSyntax, scope: Scope): BoundDefinition =
    new BoundDefinition.GlobalStatement(node.statement)

  def bindVariableDeclaration(node: MemberSyntax.VariableDeclaration, scope: Scope): BoundDefinition = {
    val parent = scope.getParentSymbol()

    val isClass = parent.kind == SymbolKind.Class

    val symbol = declareSymbol(
      SymbolKind.Field,
      parent.flags & SymbolFlags.Static,
      Declaration.FieldFromMember(node.identifier.text, node.identifier.location, node),
      scope,
      parent
    )
    new BoundDefinition.Field(symbol, node.typeAnnotation, Some(node.expression))
  }

//  def name_with_namespace(name: string): string =
//    if (ns == "") name
//    else ns + "." + name

  def declareSymbol(symbol_kind: int, flags: int, node: Declaration, scope: Scope, parent: Symbol): Symbol = {
    val name = if (symbol_kind == SymbolKind.Constructor) ".ctor" else node match {
      case Declaration.Class(name, _, _) => name
      case Declaration.ClassFromEnumCase(name, _, _) => name
      case Declaration.Constructor(name, _, _) => name
      case Declaration.FieldFromParameter(name, _, _) => name
      case Declaration.FieldFromVariable(name, _, _) => name
      case Declaration.FieldFromMember(name, _, _) => name
      case Declaration.Parameter(name, _, _) => name
      case Declaration.Local(name, _, _) => name
      case Declaration.LocalFromFor(name, _, _) => name
      case Declaration.ClassFromObject(name, _, _) => name
      case Declaration.ClassFromEnum(name, _, _) => name
      case Declaration.Method(name, _, _) => name
    }

    val location = node match {
      case Declaration.Class(_, location, _) => location
      case Declaration.ClassFromEnumCase(_, location, _) => location
      case Declaration.Constructor(_, location, _) => location
      case Declaration.FieldFromParameter(_, location, _) => location
      case Declaration.FieldFromVariable(_, location, _) => location
      case Declaration.FieldFromMember(_, location, _) => location
      case Declaration.Parameter(_, location, _) => location
      case Declaration.Local(_, location, _) => location
      case Declaration.LocalFromFor(_, location, _) => location
      case Declaration.ClassFromObject(_, location, _) => location
      case Declaration.ClassFromEnum(_, location, _) => location
      case Declaration.Method(_, location, _) => location
    }

    val res = scope.get(name)
    if (res.isEmpty) {
      val symbol = new Symbol(symbol_kind, flags, name, location, Some(parent))
      symbol.add_declaration(node)
      scope.addSymbol(symbol)
    } else {
      // TODO: add diagnostic for duplicate declaration
      res.get
    }
  }

  def bindObjectDeclaration(node: MemberSyntax.ObjectDeclarationSyntax, scope: Scope): BoundDefinition = {
    val saveParent = _parent
    val symbol = declareSymbol(SymbolKind.Class, SymbolFlags.Static, Declaration.ClassFromObject(node.identifier.text, node.identifier.location, node), scope, _parent)
    _parent = symbol

    val members = bindTemplate(node.template, symbol.members)

    _parent = saveParent

    new BoundDefinition.Object(symbol, members)
  }

  def bindEnumDeclaration(node: MemberSyntax.EnumDeclarationSyntax, scope: Scope): BoundDefinition = {
    val saveParent = _parent
    val symbol = declareSymbol(SymbolKind.Class, SymbolFlags.None, Declaration.ClassFromEnum(node.identifier.text, node.identifier.location, node), scope, _parent)
    _parent = symbol

    val cases = bindCases(node.cases, symbol.members)

    val members = bindMembers(node.members, symbol.members)

    _parent = saveParent
    new BoundDefinition.Enum(symbol, cases, members)
  }

  def bindOptionalTemplate(node: Option[TemplateSyntax], scope: Scope): BoundDefinitions =
    if (node.isDefined) bindTemplate(node.get, scope) else BoundDefinitions.Empty

  def bindTemplate(node: TemplateSyntax, scope: Scope): BoundDefinitions =
    bindMembers(node.members, scope)


  def bindClassDeclaration(node: MemberSyntax.ClassDeclarationSyntax, scope: Scope): BoundDefinition = {
    val saveParent = _parent
    val symbol = declareSymbol(SymbolKind.Class, SymbolFlags.None, Declaration.Class(node.identifier.text, node.identifier.location, node), scope, _parent)
    _parent = symbol

    val newScope = symbol.members

    //    bindConstructor(node, newScope)
    val fields = bindFields(node.parameters, newScope)
    val members = bindOptionalTemplate(node.template, newScope)

    _parent = saveParent
    new BoundDefinition.Class(symbol, fields, members)
  }

  def bindFunctionDeclaration(node: MemberSyntax.FunctionDeclarationSyntax, scope: Scope): BoundDefinition = {
    val saveParent = _parent
    val flags = _parent.flags & SymbolFlags.Static
    val isClass = _parent.kind == SymbolKind.Class
    val kind = if (isClass) SymbolKind.Method else SymbolKind.Function
    val symbol = declareSymbol(kind, flags, Declaration.Method(node.identifier.text, node.identifier.location, node), scope, _parent)
    _parent = symbol
    val methodScope = symbol.members
    val params = bindParameters(node.parameters, methodScope)

    val expr = node.body match {
      case Some(value) => Some(value.expression)
      case None => None
    }

    _parent = saveParent

    new BoundDefinition.Method(symbol, node, params, expr)
  }

  def bindParameter(node: ParameterSyntax, scope: Scope): BoundParameter = {
    val symbol = if (_parent.kind == SymbolKind.Class) {
      declareSymbol(SymbolKind.Field,
        _parent.flags & SymbolFlags.Static,
        Declaration.FieldFromParameter(node.identifier.text, node.identifier.location, node),
        scope,
        _parent
      )
    } else if (_parent.kind == SymbolKind.Method || _parent.kind == SymbolKind.Constructor) {
      declareSymbol(SymbolKind.Parameter, SymbolFlags.None, Declaration.Parameter(node.identifier.text, node.identifier.location, node), scope, _parent)
    } else {
      AstPrinter.printSymbolKind(_parent.kind)
      panic("bindParameter")
    }

    BoundParameter(symbol, node)
  }

  def bindParameters(nodes: Array[ParameterSyntax], scope: Scope): Array[BoundParameter] = {
    val params = new Array[BoundParameter](nodes.length)
    for (x <- 0 to (nodes.length - 1)) {
      params(x) = bindParameter(nodes(x), scope)
    }
    params
  }

  def bindFields(nodes: Array[ParameterSyntax], scope: Scope): Array[BoundField] = {
    val fields = new Array[BoundField](nodes.length)
    for (x <- 0 to (nodes.length - 1)) {
      fields(x) = bindFieldFromParam(scope.getParentSymbol(), nodes(x))
    }
    fields
  }
}