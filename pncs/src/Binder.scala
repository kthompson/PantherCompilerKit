import panther._
import ExpressionSyntax._
import MemberSyntax._
import SimpleNameSyntax._
import NameSyntax._

case class BoundFunction(symbol: Symbol, function: FunctionDeclarationSyntax)

case class BoundTree(root: Symbol, diagnostics: Array[Diagnostic], functions: Array[BoundFunction])

case class Binder(root: Symbol, diagnostics: DiagnosticBag) {

  var ns = "" // namespace
  var _parent = root

  var _functionsSize = 0
  var functions = new Array[BoundFunction](0)

  def bind(trees: Array[SyntaxTree]): BoundTree = {
    var num_diags = 0
    for (x <- 0 to (trees.length - 1)) {
      num_diags = num_diags + trees(x).diagnostics.length
      bindCompilationUnit(trees(x).root)
    }

    val diags = diagnostics.diagnostics()
    num_diags = num_diags + diags.length

    val allDiags = new Array[Diagnostic](num_diags)
    var diag = 0
    for (i <- 0 to (trees.length - 1)) {
      for (d <- 0 to (trees(i).diagnostics.length - 1)) {
        allDiags(diag) = trees(i).diagnostics(d)
        diag = diag + 1
      }
    }

    BoundTree(root, allDiags, boundFunctions())
  }

  def boundFunctions(): Array[BoundFunction] = {
    val newItems = new Array[BoundFunction](_functionsSize)
    for (i <- 0 to (_functionsSize - 1)) {
      newItems(i) = functions(i)
    }
    newItems
  }

  def ensureFunctionCapacity(count: int): unit = {
    if (_functionsSize + count >= functions.length) {
      val newItems = new Array[BoundFunction]((_functionsSize + count) * 2)
      for (i <- 0 to (_functionsSize - 1)) {
        newItems(i) = functions(i)
      }
      functions = newItems
    } else {
      ()
    }
  }

  def addBoundFunction(symbol: Symbol, function: FunctionDeclarationSyntax): unit = {
    ensureFunctionCapacity(1)
    functions(_functionsSize) = BoundFunction(symbol, function)
    _functionsSize = _functionsSize + 1
  }

  def bindCompilationUnit(compilationUnit: CompilationUnitSyntax): unit = {
    // bind_namespace_declarations(compilationUnit.namespaceDeclaration)
    // bind_using_declarations(compilationUnit.usings)
    bindMembers(compilationUnit.members, root.members)
  }

  def bindCases(nodes: Array[EnumCaseSyntax], scope: Scope): unit = {
    for (x <- 0 to (nodes.length - 1)) {
      bindCase(nodes(x), scope)
    }
  }

  def bindCase(node: EnumCaseSyntax, scope: Scope): unit = {
    val saveParent = _parent
    val symbol = declareSymbol(SymbolKind.Constructor, SymbolFlags.None, MakeDeclaration.enumCaseSyntax(node), scope, _parent)
    _parent = symbol

    val methodScope = symbol.members

    node.parameters match {
      case Some(parameters) => bindParameters(parameters.parameters, methodScope)
      case None => ()
    }


    _parent = saveParent
  }

  def bindMembers(nodes: Array[MemberSyntax], scope: Scope): unit = {
    for (x <- 0 to (nodes.length - 1)) {
      bindMember(nodes(x), scope)
    }
  }

  def bindMember(member: MemberSyntax, scope: Scope): unit = {
    member match {
      case value: ObjectDeclarationSyntax => bindObjectDeclaration(value, scope)
      case value: ClassDeclarationSyntax => bindClassDeclaration(value, scope)
      case value: FunctionDeclarationSyntax => bindFunctionDeclaration(value, scope)
      case value: EnumDeclarationSyntax => bindEnumDeclaration(value, scope)
      case value: GlobalStatementSyntax =>
      //                ??? // not supported yet
      //                bind_global_statement(value, scope)
    }
  }

  def name_with_namespace(name: string): string =
    if (ns == "") name
    else ns + "." + name

  def declareSymbol(symbol_kind: int, flags: int, node: Declaration, scope: Scope, parent: Symbol): Symbol = {
    val res = scope.get(node.name)
    if (res.isEmpty) {
      val symbol = new Symbol(symbol_kind, flags, node.name, node.location, Some(parent))
      symbol.add_declaration(node)
      scope.addSymbol(symbol)
    } else {
      // TODO: add diagnostic for duplicate declaration
      res.get
    }
  }

  def bindObjectDeclaration(node: ObjectDeclarationSyntax, scope: Scope): unit = {
    val saveParent = _parent
    val symbol = declareSymbol(SymbolKind.Class, SymbolFlags.Static, MakeDeclaration.member(node.identifier, node), scope, _parent)
    _parent = symbol

    bindTemplate(node.template, symbol.members)

    _parent = saveParent
  }

  def bindEnumDeclaration(node: EnumDeclarationSyntax, scope: Scope): unit = {
    val saveParent = _parent
    val symbol = declareSymbol(SymbolKind.Class, SymbolFlags.None, MakeDeclaration.member(node.identifier, node), scope, _parent)
    _parent = symbol

    bindCases(node.cases, symbol.members)

    bindMembers(node.members, symbol.members)

    _parent = saveParent
  }

  def bindOptionalTemplate(node: Option[TemplateSyntax], scope: Scope): unit =
    if (node.isDefined) bindTemplate(node.get, scope) else ()

  def bindTemplate(node: TemplateSyntax, scope: Scope): unit = {
    bindMembers(node.members, scope)
  }

  def bindClassDeclaration(node: ClassDeclarationSyntax, scope: Scope): unit = {
    val saveParent = _parent
    val symbol = declareSymbol(SymbolKind.Class, SymbolFlags.None, MakeDeclaration.member(node.identifier, node), scope, _parent)
    _parent = symbol

    val newScope = symbol.members

    bindConstructor(node, newScope)
    bindFields(node.parameters, newScope)
    bindOptionalTemplate(node.template, newScope)

    _parent = saveParent
  }

  def bindConstructor(node: ClassDeclarationSyntax, scope: Scope): unit = {
    val saveParent = _parent
    val symbol = declareSymbol(SymbolKind.Constructor, SymbolFlags.None, MakeDeclaration.member(node.identifier, node), scope, _parent)
    _parent = symbol

    val methodScope = symbol.members

    bindParameters(node.parameters, methodScope)

    _parent = saveParent
  }

  def bindFunctionDeclaration(node: FunctionDeclarationSyntax, scope: Scope): unit = {
    val newScope = if (scope.isGlobalScope()) {
      // TODO: need to adjust parent here as we are moving our parent symbol
      program_scope(scope)
    } else {
      scope
    }

    val saveParent = _parent
    val flags = _parent.flags & SymbolFlags.Static
    val symbol = declareSymbol(SymbolKind.Method, flags, MakeDeclaration.member(node.identifier, node), scope, _parent)

    _parent = symbol

    val methodScope = symbol.members

    bindParameters(node.parameters, methodScope)
    //        bind_optional_function_body(node.body, methodScope)

    addBoundFunction(symbol, node)

    _parent = saveParent
  }


  def program_symbol(scope: Scope): Symbol = {
    val maybeProgram = scope.get("$Program")
    if (maybeProgram.isEmpty) {
      scope.addSymbol(new Symbol(SymbolKind.Class, SymbolFlags.Static, "$Program", TextLocationFactory.empty(), None))
    } else {
      maybeProgram.get
    }
  }

  def program_scope(scope: Scope): Scope =
    program_symbol(scope).members

  //    def bind_global_statement(node: GlobalStatementSyntax, scope: Scope): unit = {
  //        val newScope = if (scope.isGlobalScope()) {
  //            val program = program_symbol(scope)
  //            val programScope = program.members
  //            val maybeMain = programScope.get("main")
  //            if (maybeMain.isEmpty) {
  //                val parent = Some(program)
  //                programScope.
  //                    addSymbol(new Symbol(SymbolKind.Method, SymbolFlags.Static, "main", TextLocationFactory.empty(), parent)).
  //                    members
  //            } else {
  //                // TODO: diagnostic cant have main and global statements
  //                maybeMain.get.members
  //            }
  //        } else {
  //            scope
  //        }
  //
  //        bind_statment(node.statement, newScope)
  //    }

  def bind_parameter(node: ParameterSyntax, scope: Scope): unit =
    declareSymbol(SymbolKind.Parameter, SymbolFlags.None, MakeDeclaration.parameter(node), scope, _parent)

  //    def bind_optional_function_body(node: Option[FunctionBodySyntax], scope: Scope): unit =
  //        if (node.isDefined) bind_function_body(node.get, scope) else ()

  //    def bind_function_body(node: FunctionBodySyntax, scope: Scope): unit = {
  //        bindExpression(node.expression, scope)
  //    }

  //    def bindExpression(node: ExpressionSyntax, scope: Scope): unit = {
  //      node match {
  //        case ArrayCreationExpression(value) => bind_array_creation_expression(value, scope)
  //        case AssignmentExpression(value) => bind_assignment_expression(value, scope)
  //        case BinaryExpression(value) => bindBinaryExpression(value, scope)
  //        case BlockExpression(value) => bindBlockExpression(value, scope)
  //        case CallExpression(value) => bindCallExpression(value, scope)
  //        case ForExpression(value) => bindForExpression(value, scope)
  //        case GroupExpression(value) => bindGroupExpression(value, scope)
  //        case IdentifierName(value) => bind_identifier_name(value, scope)
  //        case IfExpression(value) => bindIfExpression(value, scope)
  //        case IndexExpression(value) => bindIndexExpression(value, scope)
  //        case LiteralExpression(value) => bind_literal_expression(value, scope)
  //        case value: MatchExpression => bindMatchExpression(value, scope)
  //        case MemberAccessExpression(value) => bind_member_access_expression(value, scope)
  //        case NewExpression(value) => bind_new_expression(value, scope)
  //        case UnaryExpression(value) => bind_unary_expression(value, scope)
  //        case UnitExpression(value) => bind_unit_expression(value, scope)
  //        case WhileExpression(value) => bind_while_expression(value, scope)
  //      }
  //    }

  //    def bindMatchExpression(node: MatchExpression, scope: Scope): unit = {
  //        bindExpression(node.expression, scope)
  //        bindMatchCases(node.cases, scope)
  //    }

  //    def bindMatchCases(nodes: Array[MatchCaseSyntax], scope: Scope): unit = {
  //        for (x <- 0 to (nodes.length-1)) {
  //            bindMatchCase(nodes(x), x, scope)
  //        }
  //    }

  //    def bindMatchCase(node: MatchCaseSyntax, index: int, scope: Scope): unit = {
  //        val matchScope = scope.openScope("match" + string(index))
  //        bindMatchPattern(node.pattern, matchScope)
  //        bindBlockExpressionList(node.block, matchScope)
  //    }

  //    def bindMatchPattern(node: PatternSyntax, scope: Scope): unit = {
  //        node match {
  //            case PatternSyntax.DiscardPattern(value) => ()
  //            case PatternSyntax.LiteralPattern(value) => ()
  //            case PatternSyntax.TypePattern(value, annotation) =>
  //                // TODO: need to provide the type annotation to the symbol so we can detect the type
  //                // in the checker
  //                declareSymbol(SymbolKind.Local, SymbolFlags.None, MakeDeclaration.token(value), scope, _parent)
  //            case PatternSyntax.ExtractPattern(name, _, patterns, _) =>
  //                // TODO: need to bind the name
  //                bindMatchPatterns(patterns, scope)
  //        }
  //    }
  //
  //    def bindMatchPatterns(nodes: Array[PatternItemSyntax], scope: Scope): unit = {
  //        for (x <- 0 to (nodes.length-1)) {
  //            bindMatchPattern(nodes(x).pattern, scope)
  //        }
  //    }

  def bind_identifier_name(node: IdentifierNameSyntax, scope: Scope): unit = {
    // TODO: ensure symbol exists
  }

  //    def bind_array_creation_expression(node: ArrayCreationExpressionSyntax, scope: Scope): unit = {
  //        get_name(node.name)
  //        bindOptionalExpression(node.arrayRank, scope)
  //        bind_array_initializer_expressions(node.initializer, scope)
  //    }
  //
  //    def bind_array_initializer_expressions(node: Option[ArrayInitializerExpressionSyntax], scope: Scope): unit = {
  //        if (node.isDefined) {
  //            bind_array_initializer_expression(node.get, scope)
  //        } else ()
  //    }
  //    def bind_array_initializer_expression(node: ArrayInitializerExpressionSyntax, scope: Scope): unit = {
  //        bindExpressionList(node.expressions, scope)
  //    }

  //    def bind_assignment_expression(node: AssignmentExpressionSyntax, scope: Scope): unit = {
  //        bindExpression(node.left, scope)
  //        bindExpression(node.right, scope)
  //    }

  //    def bindBinaryExpression(node: BinaryExpressionSyntax, scope: Scope): unit = {
  //        bindExpression(node.left, scope)
  //        bindExpression(node.right, scope)
  //    }

  //    def bindBlockExpression(node: BlockExpressionSyntax, scope: Scope): unit = {
  //        val blockScope = scope.openScope("block")
  //        bindBlockExpressionList(node.block, blockScope)
  //    }

  //    def bindBlockExpressionList(node: BlockExpressionListSyntax, blockScope: Scope): Unit = {
  //        bindStatments(node.statements, blockScope)
  //        bindOptionalExpression(node.expression, blockScope)
  //    }

  //    def bindCallExpression(node: CallExpressionSyntax, scope: Scope): unit = {
  //        bindExpression(node.name, scope)
  //        bindExpressionList(node.arguments, scope)
  //    }

  //    def bindForExpression(node: ForExpressionSyntax, scope: Scope): unit = {
  //        val forScope = scope.openScope("for")
  //
  //        val saveParent = _parent
  //        val symbol = declareSymbol(SymbolKind.Local, SymbolFlags.None, MakeDeclaration.token(node.identifier), forScope, _parent)
  //        _parent = symbol
  //
  //        bindExpression(node.fromExpr, scope)
  //        bindExpression(node.toExpr, scope)
  //        bindExpression(node.body, forScope)
  //
  //        _parent = saveParent
  //    }

  //    def bindGroupExpression(node: GroupExpressionSyntax, scope: Scope): unit =
  //        bindExpression(node.expression, scope)

  //    def bindIfExpression(node: IfExpressionSyntax, scope: Scope): unit = {
  //        bindExpression(node.condition, scope)
  //        bindExpression(node.thenExpr, scope)
  //        bindExpression(node.elseExpr, scope)
  //    }

  //    def bindIndexExpression(node: IndexExpressionSyntax, scope: Scope): unit = {
  //        bindExpression(node.left, scope)
  //        bindExpression(node.index, scope)
  //    }
  //    def bind_literal_expression(node: LiteralExpressionSyntax, scope: Scope): unit = {
  //    }
  //    def bind_member_access_expression(node: MemberAccessExpressionSyntax, scope: Scope): unit = {
  //        bindExpression(node.left, scope)
  //        // bind_expression(node.right, scope)
  //    }
  //    def bind_new_expression(node: NewExpressionSyntax, scope: Scope): unit = {
  //        get_name(node.name)
  //        bindExpressionList(node.arguments, scope)
  //    }
  //    def bind_unary_expression(node: UnaryExpressionSyntax, scope: Scope): unit = {
  //        bindExpression(node.expression, scope)
  //    }
  //    def bind_unit_expression(node: UnitExpressionSyntax, scope: Scope): unit = {
  //    }
  //    def bind_while_expression(node: WhileExpressionSyntax, scope: Scope): unit = {
  //        bindExpression(node.condition, scope)
  //        bindExpression(node.body, scope)
  //    }
  //
  //    def bindExpressionList(node: ExpressionListSyntax, scope: Scope): unit = {
  //        bind_expression_items(node.expressions, scope)
  //    }

  //    def bind_expression_items(nodes: Array[ExpressionItemSyntax], scope: Scope): unit = {
  //        for (x <- 0 to (nodes.length-1)) {
  //            bind_expression_item(nodes(x), scope)
  //        }
  //    }

  //    def bind_expression_item(node: ExpressionItemSyntax, scope: Scope): unit =
  //        bindExpression(node.expression, scope)

  //    def bind_statment(node: StatementSyntax, scope: Scope): unit = {
  //        node match {
  //            case StatementSyntax.VariableDeclarationStatement(value) => bind_variable_declaration_statement(value, scope)
  //            case StatementSyntax.BreakStatement(value) => bind_break_statement(value, scope)
  //            case StatementSyntax.ContinueStatement(value) => bind_continue_statement(value, scope)
  //            case StatementSyntax.ExpressionStatement(value) => bind_expression_statement(value, scope)
  //        }
  //    }
  //
  //    def bind_variable_declaration_statement(node: VariableDeclarationStatementSyntax, scope: Scope): unit = {
  //        val parent = scope.getParentSymbol()
  //
  //        if (parent.kind == SymbolKind.Class) {
  //            declareSymbol(SymbolKind.Field, parent.flags & SymbolFlags.Static, MakeDeclaration.local(node), scope, _parent)
  //            ()
  //        } else if (parent.kind == SymbolKind.Method) {
  //            declareSymbol(SymbolKind.Local, SymbolFlags.None, MakeDeclaration.local(node), scope, _parent)
  //            ()
  //        } else {
  //            panic("bind_variable_declaration_statement")
  //        }
  //
  //        bindExpression(node.expression, scope)
  //    }
  //
  //    def bind_break_statement(node: BreakStatementSyntax, scope: Scope): unit = ()
  //    def bind_continue_statement(node: ContinueStatementSyntax, scope: Scope): unit = ()
  //    def bind_expression_statement(node: ExpressionStatementSyntax, scope: Scope): unit = {
  //        bindExpression(node.expression, scope)
  //    }
  //
  //    def bindStatments(nodes: Array[StatementSyntax], scope: Scope): unit = {
  //        for (x <- 0 to (nodes.length-1)) {
  //            bind_statment(nodes(x), scope)
  //        }
  //    }

  //    def bindOptionalExpression(node: Option[ExpressionSyntax], scope: Scope): unit = {
  //        if (node.isDefined) {
  //            bindExpression(node.get, scope)
  //        } else ()
  //    }
  //
  //    def bind_expressions(nodes: Array[ExpressionSyntax], scope: Scope): unit = {
  //        for (x <- 0 to (nodes.length-1)) {
  //            bindExpression(nodes(x), scope)
  //        }
  //    }

  def bindParameters(nodes: Array[ParameterSyntax], scope: Scope): unit = {
    for (x <- 0 to (nodes.length - 1)) {
      bind_parameter(nodes(x), scope)
    }
  }

  def bindFields(nodes: Array[ParameterSyntax], scope: Scope): unit = {
    for (x <- 0 to (nodes.length - 1)) {
      bindField(nodes(x), scope)
    }
  }

  def bindField(node: ParameterSyntax, scope: Scope): unit =
    declareSymbol(SymbolKind.Field, _parent.flags & SymbolFlags.Static, MakeDeclaration.parameter(node), scope, _parent)

  def get_name(name: NameSyntax): string = {
    name match {
      case value: NameSyntax.QualifiedNameSyntax =>
        get_qualified_name(value)
      case NameSyntax.SimpleName(value) =>
        get_simple_name(value)
    }
  }

  def get_qualified_name(node: QualifiedNameSyntax): string =
    get_name(node.left) + "." + get_simple_name(node.right)

  def get_generic_name(node: GenericNameSyntax): string =
    node.identifier.text + get_type_argument_list(node.typeArgumentlist)

  def get_simple_name(node: SimpleNameSyntax): string = {
    node match {
      case value: SimpleNameSyntax.GenericNameSyntax => get_generic_name(value)
      case value: SimpleNameSyntax.IdentifierNameSyntax => get_identifier_name(value)
    }
  }

  def get_type_argument_lists(nodes: Array[TypeArgumentListSyntax], scope: Scope): string = {
    var res = "<"
    for (x <- 0 to (nodes.length - 1)) {
      res = res + get_type_argument_list(nodes(x))
    }
    res + ">"
  }

  def get_type_argument_list(node: TypeArgumentListSyntax): string =
    get_type_argument_list_items(node.arguments)

  def get_type_argument_list_items(nodes: Array[TypeArgumentItemSyntax]): string = {
    var res = ""
    for (x <- 0 to (nodes.length - 1)) {
      res = res + bind_type_argument_list_item(nodes(x))
    }
    res
  }

  def bind_type_argument_list_item(node: TypeArgumentItemSyntax): string = {
    val sep =
      if (node.separator.isDefined) ","
      else ""
    get_name(node.name) + sep
  }

  def bind_identifier_names(nodes: Array[IdentifierNameSyntax], scope: Scope): unit = {
    for (x <- 0 to (nodes.length - 1)) {
      bind_identifier_name(nodes(x), scope)
    }
  }

  def get_identifier_name(node: IdentifierNameSyntax): string =
    node.identifier.text
}