import panther._

case class Binder(root: Symbol, diagnostics: DiagnosticBag) {

    var ns = "" // namespace
    var _parent = root

    def bind_compilation_unit(compilationUnit: CompilationUnitSyntax): unit = {
        // bind_namespace_declarations(compilationUnit.namespaceDeclaration)
        // bind_using_declarations(compilationUnit.usings)
        bind_members(compilationUnit.members, root.members)
    }

    def bind_members(nodes: Array[MemberSyntax], scope: Scope): unit = {
        for (x <- 0 to (nodes.length-1)) {
            bind_member(nodes(x), scope)
        }
    }

    def bind_member(member: MemberSyntax, scope: Scope): unit = {
        val kind = member.kind
        if (kind == SyntaxKind.ObjectDeclaration) {
            bind_object_declaration(member.objekt.get, scope)
        } else if (kind == SyntaxKind.ClassDeclaration) {
            bind_class_declaration(member.klass.get, scope)
        } else if (kind == SyntaxKind.FunctionDeclaration) {
            bind_function_declaration(member.function.get, scope)
        } else if (kind == SyntaxKind.GlobalStatement) {
            bind_global_statement(member.statement.get, scope)
        } else panic("bind_member")
    }

    def name_with_namespace(name: string): string =
        if (ns == "") name
        else ns + "." + name

    def declare_symbol(symbol_kind: int, node: Declaration, scope: Scope, parent: Symbol): Symbol = {
        val res = scope.get(node.name)
        if (res.isEmpty) {
            val symbol = new Symbol(symbol_kind, node.name, node.location, Some(parent))
            symbol.add_declaration(node)
            scope.add_symbol(symbol)
        } else {
            // TODO: add diagnostic for duplicate declaration
            res.get
        }
    }

    def bind_object_declaration(node: ObjectDeclarationSyntax, scope: Scope): unit = {
        val saveParent = _parent
        val symbol = declare_symbol(SymbolKind.Class, MakeDeclaration.obj(node), scope, _parent)
        _parent = symbol

        bind_template(node.template, symbol.members)

        _parent = saveParent
    }

    def bind_optional_template(node: Option[TemplateSyntax], scope: Scope): unit =
        if (node.isDefined) bind_template(node.get, scope) else ()

    def bind_template(node: TemplateSyntax, scope: Scope): unit = {
        bind_members(node.members, scope)
    }

    def bind_class_declaration(node: ClassDeclarationSyntax, scope: Scope): unit = {
        val saveParent = _parent
        val symbol = declare_symbol(SymbolKind.Class, MakeDeclaration.cls(node), scope, _parent)
        _parent = symbol

        val newScope = symbol.members

        bind_constructor(node, node.parameters, newScope)
        bind_fields(node.parameters, newScope)
        bind_optional_template(node.template, newScope)

        _parent = saveParent
    }

    def bind_constructor(node: ClassDeclarationSyntax, parameters: Array[ParameterSyntax], scope: Scope): unit = {
        val saveParent = _parent
        val symbol = declare_symbol(SymbolKind.Constructor, MakeDeclaration.cls(node), scope, _parent)
        _parent = symbol

        val methodScope = symbol.members

        bind_parameters(parameters, methodScope)

        _parent = saveParent
    }

    def bind_function_declaration(node: FunctionDeclarationSyntax, scope: Scope): unit = {
        val newScope = if (scope.is_global_scope()) {
            // TODO: need to adjust parent here as we are moving our parent symbol
            program_scope(scope)
        } else {
            scope
        }

        val saveParent = _parent
        val symbol = declare_symbol(SymbolKind.Method, MakeDeclaration.function(node), scope, _parent)
        _parent = symbol

        val methodScope = symbol.members

        bind_parameters(node.parameters, methodScope)
        bind_optional_function_body(node.body, methodScope)

        _parent = saveParent
    }


    def program_symbol(scope: Scope): Symbol = {
        val maybeProgram = scope.get("$Program")
        if (maybeProgram.isEmpty) {
            scope.add_symbol(new Symbol(SymbolKind.Class, "$Program", TextLocationFactory.empty(), None))
        } else {
            maybeProgram.get
        }
    }

    def program_scope(scope: Scope): Scope =
        program_symbol(scope).members

    def bind_global_statement(node: GlobalStatementSyntax, scope: Scope): unit = {
        val newScope = if (scope.is_global_scope()) {
            val program = program_symbol(scope)
            val programScope = program.members
            val maybeMain = programScope.get("main")
            if (maybeMain.isEmpty) {
                val parent = Some(program)
                programScope.
                    add_symbol(new Symbol(SymbolKind.Method, "main", TextLocationFactory.empty(), parent)).
                    members
            } else {
                // TODO: diagnostic cant have main and global statements
                maybeMain.get.members
            }
        } else {
            scope
        }

        bind_statment(node.statement, newScope)
    }

    def bind_parameter(node: ParameterSyntax, scope: Scope): unit =
        declare_symbol(SymbolKind.Parameter, MakeDeclaration.parameter(node), scope, _parent)

    def bind_optional_function_body(node: Option[FunctionBodySyntax], scope: Scope): unit =
        if (node.isDefined) bind_function_body(node.get, scope) else ()

    def bind_function_body(node: FunctionBodySyntax, scope: Scope): unit = {
        bind_expression(node.expression, scope)
    }

    def bind_expression(node: ExpressionSyntax, scope: Scope): unit = {
        val kind = node.kind
        if (kind == SyntaxKind.ArrayCreationExpression) {
            bind_array_creation_expression(node.arrayCreationExpression.get, scope)
        } else if (kind == SyntaxKind.AssignmentExpression) {
            bind_assignment_expression(node.assignmentExpression.get, scope)
        } else if (kind == SyntaxKind.BinaryExpression) {
            bind_binary_expression(node.binaryExpression.get, scope)
        } else if (kind == SyntaxKind.BlockExpression) {
            bind_block_expression(node.blockExpression.get, scope)
        } else if (kind == SyntaxKind.CallExpression) {
            bind_call_expression(node.callExpression.get, scope)
        } else if (kind == SyntaxKind.ForExpression) {
            bind_for_expression(node.forExpression.get, scope)
        } else if (kind == SyntaxKind.GroupExpression) {
            bind_group_expression(node.groupExpression.get, scope)
        } else if (kind == SyntaxKind.IdentifierName) {
            bind_identifier_name(node.identifierName.get)
        } else if (kind == SyntaxKind.IfExpression) {
            bind_if_expression(node.ifExpression.get, scope)
        } else if (kind == SyntaxKind.IndexExpression) {
            bind_index_expression(node.indexExpression.get, scope)
        } else if (kind == SyntaxKind.LiteralExpression) {
            bind_literal_expression(node.literalExpression.get, scope)
        } else if (kind == SyntaxKind.MemberAccessExpression) {
            bind_member_access_expression(node.memberAccessExpression.get, scope)
        } else if (kind == SyntaxKind.NewExpression) {
            bind_new_expression(node.newExpression.get, scope)
        } else if (kind == SyntaxKind.UnaryExpression) {
            bind_unary_expression(node.unaryExpression.get, scope)
        } else if (kind == SyntaxKind.UnitExpression) {
            bind_unit_expression(node.unitExpression.get, scope)
        } else if (kind == SyntaxKind.WhileExpression) {
            bind_while_expression(node.whileExpression.get, scope)
        } else ()
    }

    def bind_identifier_name(node: IdentifierNameSyntax): unit = {
        // TODO: ensure symbol exists
    }

    def bind_array_creation_expression(node: ArrayCreationExpressionSyntax, scope: Scope): unit = {
        get_name(node.name)
        bind_optional_expression(node.arrayRank, scope)
        bind_array_initializer_expressions(node.initializer, scope)
    }

    def bind_array_initializer_expressions(node: Option[ArrayInitializerExpressionSyntax], scope: Scope): unit = {
        if (node.isDefined) {
            bind_array_initializer_expression(node.get, scope)
        } else ()
    }
    def bind_array_initializer_expression(node: ArrayInitializerExpressionSyntax, scope: Scope): unit = {
        bind_expression_list(node.expressions, scope)
    }

    def bind_assignment_expression(node: AssignmentExpressionSyntax, scope: Scope): unit = {
        bind_expression(node.left, scope)
        bind_expression(node.right, scope)
    }

    def bind_binary_expression(node: BinaryExpressionSyntax, scope: Scope): unit = {
        bind_expression(node.left, scope)
        bind_expression(node.right, scope)
    }

    def bind_block_expression(node: BlockExpressionSyntax, scope: Scope): unit = {
        val blockScope = scope.open_scope("block")
        bind_statments(node.statements, blockScope)
        bind_optional_expression(node.expression, blockScope)
    }

    def bind_call_expression(node: CallExpressionSyntax, scope: Scope): unit = {
        bind_expression(node.name, scope)
        bind_expression_list(node.arguments, scope)
    }

    def bind_for_expression(node: ForExpressionSyntax, scope: Scope): unit = {
        val forScope = scope.open_scope("for")

        val saveParent = _parent
        val symbol = declare_symbol(SymbolKind.Local, MakeDeclaration.token(node.identifier), forScope, _parent)
        _parent = symbol

        bind_expression(node.fromExpr, scope)
        bind_expression(node.toExpr, scope)
        bind_expression(node.body, forScope)

        _parent = saveParent
    }

    def bind_group_expression(node: GroupExpressionSyntax, scope: Scope): unit =
        bind_expression(node.expression, scope)

    def bind_if_expression(node: IfExpressionSyntax, scope: Scope): unit = {
        bind_expression(node.condition, scope)
        bind_expression(node.thenExpr, scope)
        bind_expression(node.elseExpr, scope)
    }

    def bind_index_expression(node: IndexExpressionSyntax, scope: Scope): unit = {
        bind_expression(node.left, scope)
        bind_expression(node.index, scope)
    }
    def bind_literal_expression(node: LiteralExpressionSyntax, scope: Scope): unit = {
    }
    def bind_member_access_expression(node: MemberAccessExpressionSyntax, scope: Scope): unit = {
        bind_expression(node.left, scope)
        // bind_expression(node.right, scope)
    }
    def bind_new_expression(node: NewExpressionSyntax, scope: Scope): unit = {
        get_name(node.identifier)
        bind_expression_list(node.arguments, scope)
    }
    def bind_unary_expression(node: UnaryExpressionSyntax, scope: Scope): unit = {
        bind_expression(node.expression, scope)
    }
    def bind_unit_expression(node: UnitExpressionSyntax, scope: Scope): unit = {
    }
    def bind_while_expression(node: WhileExpressionSyntax, scope: Scope): unit = {
        bind_expression(node.left, scope)
        bind_expression(node.body, scope)
    }

    def bind_expression_list(node: ExpressionListSyntax, scope: Scope): unit = {
        bind_expression_items(node.expressions, scope)
    }

    def bind_expression_items(nodes: Array[ExpressionItemSyntax], scope: Scope): unit = {
        for (x <- 0 to (nodes.length-1)) {
            bind_expression_item(nodes(x), scope)
        }
    }

    def bind_expression_item(node: ExpressionItemSyntax, scope: Scope): unit =
        bind_expression(node.expression, scope)

    def bind_statment(node: StatementSyntax, scope: Scope): unit = {
        val kind = node.kind
        if (kind == SyntaxKind.VariableDeclarationStatement) {
            bind_variable_declaration_statement(node.variableDeclarationStatement.get, scope)
        } else if (kind == SyntaxKind.BreakStatement) {
            bind_break_statement(node.breakStatement.get, scope)
        } else if (kind == SyntaxKind.ContinueStatement) {
            bind_continue_statement(node.continueStatement.get, scope)
        } else if (kind == SyntaxKind.ExpressionStatement) {
            bind_expression_statement(node.expressionStatement.get, scope)
        } else ()
    }

    def bind_variable_declaration_statement(node: VariableDeclarationStatementSyntax, scope: Scope): unit = {
        val parent = scope.get_parent_symbol()

        if (parent.kind == SymbolKind.Class) {
            declare_symbol(SymbolKind.Field, MakeDeclaration.local(node), scope, _parent)
            ()
        } else if (parent.kind == SymbolKind.Method) {
            declare_symbol(SymbolKind.Local, MakeDeclaration.local(node), scope, _parent)
            ()
        } else {
            panic("bind_variable_declaration_statement")
        }

        bind_expression(node.expression, scope)
    }

    def bind_break_statement(node: BreakStatementSyntax, scope: Scope): unit = ()
    def bind_continue_statement(node: ContinueStatementSyntax, scope: Scope): unit = ()
    def bind_expression_statement(node: ExpressionStatementSyntax, scope: Scope): unit = {
        bind_expression(node.expression, scope)
    }

    def bind_statments(nodes: Array[StatementSyntax], scope: Scope): unit = {
        for (x <- 0 to (nodes.length-1)) {
            bind_statment(nodes(x), scope)
        }
    }

    def bind_optional_expression(node: Option[ExpressionSyntax], scope: Scope): unit = {
        if (node.isDefined) {
            bind_expression(node.get, scope)
        } else ()
    }

    def bind_expressions(nodes: Array[ExpressionSyntax], scope: Scope): unit = {
        for (x <- 0 to (nodes.length-1)) {
            bind_expression(nodes(x), scope)
        }
    }

    def bind_parameters(nodes: Array[ParameterSyntax], scope: Scope): unit = {
        for (x <- 0 to (nodes.length-1)) {
            bind_parameter(nodes(x), scope)
        }
    }

    def bind_fields(nodes: Array[ParameterSyntax], scope: Scope): unit = {
        for (x <- 0 to (nodes.length-1)) {
            bind_field(nodes(x), scope)
        }
    }

    def bind_field(node: ParameterSyntax, scope: Scope): unit =
        declare_symbol(SymbolKind.Field, MakeDeclaration.parameter(node), scope, _parent)

    def get_name(name: NameSyntax): string = {
        val kind = name.kind
        if (kind == SyntaxKind.QualifiedName) {
            get_qualified_name(name.qualifiedName.get)
        } else if (kind == SyntaxKind.SimpleName) {
            get_simple_name(name.simpleName.get)
        } else {
            panic("invalid NameSyntax kind")
            ""
        }
    }

    def get_qualified_name(node: QualifiedNameSyntax): string =
        get_name(node.left) + "." + get_simple_name(node.right)

    def get_generic_name(node: GenericNameSyntax): string =
        node.identifier.text + get_type_argument_list(node.typeArgumentlist)

    def get_simple_name(node: SimpleNameSyntax): string = {
        val kind = node.kind
        if (kind == SyntaxKind.GenericName) {
            get_generic_name(node.genericName.get)
        } else if (kind == SyntaxKind.IdentifierName) {
            get_identifier_name(node.identifierName.get)
        } else {
            panic("invalid kind for SimpleNameSyntax: " + SyntaxFacts.get_kind_name(kind))
        }
    }

    def get_type_argument_lists(nodes: Array[TypeArgumentListSyntax], scope: Scope): string = {
        var res = "<"
        for (x <- 0 to (nodes.length-1)) {
            res = res + get_type_argument_list(nodes(x))
        }
        res + ">"
    }

    def get_type_argument_list(node: TypeArgumentListSyntax): string =
        get_type_argument_list_items(node.arguments)

    def get_type_argument_list_items(nodes: Array[TypeArgumentItemSyntax]): string = {
        var res = ""
        for (x <- 0 to (nodes.length-1)) {
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
        for (x <- 0 to (nodes.length-1)) {
            bind_identifier_name(nodes(x))
        }
    }

    def get_identifier_name(node: IdentifierNameSyntax): string =
        node.identifier.text
}