import panther._

// object TypeSchemeKind {
//     val Polymorphic = 1
//     val Simple = 2
// }

// class TypeScheme(kind: int, polymorphic: Array[PolymorphicType], simple: Array[SimpleType])

// class PolymorphicType(body: SimpleType)

// object SimpleTypeKind {
//     val Variable = 1
//     val Concrete = 2
// }
// class SimpleType(kind: int, variable: Array[Variable], concrete: Array[ConcreteType]) {
//     def is_any(): bool = kind == SimpleTypeKind.Concrete && concrete[0].kind == ConcreteTypeKind.Any
//     def is_nothing(): bool = kind == SimpleTypeKind.Concrete && concrete[0].kind == ConcreteTypeKind.Nothing
// }

// object ConcreteTypeKind {
//     val Any = 1 // Top
//     val Nothing = 2 // Bottom
//     val Function = 3
//     val Record = 4
//     val Primitive = 5
//     val Array = 5
// }

// class ConcreteType(
//     kind: int,
//     function: Array[Function],
//     record: Array[Record],
//     primitive: Array[Primitive],
//     variable: Array[Variable]
// )

// class Function(params: Array[SimpleType], returnType: SimpleType)
// class Array(type: Type)
// class RecordField(name: string, type: SimpleType)
// class Record(fields: Array[RecordField])
// class Primitive(name: string)
// class Variable(lower: ConcreteType, upper: ConcreteType) {
//     def lower_bound(): SimpleType = {}
//     def upper_bound(): SimpleType = { panic() }
//     def as_type_var(): Type = {}
// }

// Any is Top (super type of all types)
// Nothing is Bottom (sub type of all types)

// https://github.com/LPTK/simpler-sub/blob/simpler-sub/shared/src/main/scala/simplesub/Typer.scala

// ∩ == intersection
// ∪ == union
//
// https://www.youtube.com/watch?v=d10q-b8jNKg
// x gets type variable A
// y gets type variable B

// union of branches gets type variable C

// constraint: A <= int
// constraint: B <= int
// constraint: A, B <= C

// f: A -> B -> C where A <= int, B <= int, A ∪ B <= C
// then (coalesce bounds)
// f: A ∩ int -> B ∩ int -> A ∪ B ∪ C
// then simplify variables
// f: A ∩ int -> A ∩ int -> A

case class SymbolLinks() {
    var typing = false
    var _type: Option[Type] = None

    def set_type(typ: Type): Type = {
        _type = Some(typ)
        typ
    }

    def has_type(): bool = _type.isDefined
    def get_type(): Type = _type.get
}

// will be connected to a Declaration or an Expression
case class NodeLinks() {
    var _symbol: Option[Symbol] = None
    def has_symbol(): bool = _symbol.isDefined
    def get_symbol(): Symbol = _symbol.get
    def set_symbol(symbol: Symbol): Symbol = {
        _symbol = Some(symbol)
        symbol
    }

    var _type: Option[Type] = None
    def has_type(): bool = _type.isDefined
    def get_type(): Type = _type.get
    def set_type(typ: Type): Type = {
        _type = Some(typ)
        typ
    }
}

case class TypeLinks() {
    var resolved: bool = false
    var resolving: bool = false
}

case class Checker(root: Symbol) {

    val BoolType: Type = MakeType.primitive("bool")
    val CharType: Type = MakeType.primitive("char")
    val IntType: Type = MakeType.primitive("int")
    val ErrorType: Type = MakeType.primitive("err")
    val StringType: Type = MakeType.primitive("string")
    val UnitType: Type = MakeType.primitive("unit")
    val AnyType: Type = MakeType.primitive("any")

    // val Any = new ConcreteType(
    //     ConcreteTypeKind.Any,
    //     new Array[Function](0),
    //     new Array[Record](0),
    //     new Array[Primitive](0),
    //     new Array[Variable](0)
    // )

    // val Nothing = new ConcreteType(
    //     ConcreteTypeKind.Nothing,
    //     new Array[Function](0),
    //     new Array[Record](0),
    //     new Array[Primitive](0),
    //     new Array[Variable](0)
    // )

    var last_symbol_id = 0
    var symbol_links = new Array[SymbolLinks](10)
    var last_node_id = 0
    var node_links = new Array[NodeLinks](10)
    var last_type_id = 0
    var type_links = new Array[TypeLinks](10)

    def infer_types(root: Symbol): unit = {

    }

    // def fresh_var(): Variable = new Variable(Nothing, Any)

    def get_return_type_of_symbol(symbol: Symbol): Type = {
        val typ = get_type_of_symbol(symbol)
        get_return_type(typ)
    }

    def get_return_type(typ: Type): Type = {
        if (typ.kind == TypeKind.Function) {
            typ.function.get.returnType
        } else {
            panic("get_return_type expected function kind, found: " + string(typ.kind))
            ErrorType
        }
    }

    def resolveTypeConstructor(ctor: TypeConstructor): bool = {
        var resolved = true
        for (i <- 0 to (ctor.parameters.length-1)) {
            resolved = resolved && resolve_type(ctor.parameters(i))
        }
        resolved
    }

    def resolve_function_type(func: FunctionType): bool = {
        var resolved = true
        for (i <- 0 to (func.parameters.length-1)) {
            resolved = resolved && resolve_type(func.parameters(i))
        }
        resolved && resolve_type(func.returnType)
    }

    def resolve_array_type(array: ArrayType): bool =
        resolve_type(array.inner)

    def resolve_option_type(option: OptionType): bool =
        resolve_type(option.inner)

    def resolve_unresolved_type(typ: Type): bool = {
        val symbol = typ.symbol.get
        val resolved = get_type_of_symbol(symbol)
        if (resolved.kind == TypeKind.Unresolved) {
            // noop
            false
        } else {
            typ.id = resolved.id
            typ.kind = resolved.kind
            typ.typeConstructor = resolved.typeConstructor
            typ.intersection = resolved.intersection
            typ.primitive = resolved.primitive
            typ.typeVariable = resolved.typeVariable
            typ.function = resolved.function
            typ.array = resolved.array
            typ.option = resolved.option
            val links = get_type_links(typ)
            links.resolved
        }
    }

    def getTypeName(typ: Type): string = {
        if (typ.kind == TypeKind.Primitive) {
            typ.primitive.get.name
        } else if (typ.kind == TypeKind.TypeConstructor) {
            typ.symbol.get.name
        } else if (typ.kind == TypeKind.Array) {
            "Array<" + getTypeName(typ.array.get.inner) + ">"
        } else if (typ.kind == TypeKind.Option) {
            "Option<" + getTypeName(typ.option.get.inner) + ">"
        } else if (typ.kind == TypeKind.Function) {
            val params = typ.function.get.parameters.map(getTypeName).mkString(", ")
            val returnType = getTypeName(typ.function.get.returnType)
            s"($params) -> $returnType"
        } else {
            panic("getTypeName: " + typ.kind)
            "unknown"
        }
    }

    def resolve_type(typ: Type): bool = {
        val links = get_type_links(typ)

        val start = links.resolved
        val resolved = if (links.resolving || links.resolved || typ.kind == TypeKind.Primitive) {
            true
        } else {
            links.resolving = true

            if (typ.kind == TypeKind.TypeConstructor) {
                resolveTypeConstructor(typ.typeConstructor.get)
            } else if (typ.kind == TypeKind.Array) {
                resolve_array_type(typ.array.get)
            } else if (typ.kind == TypeKind.Option) {
                resolve_option_type(typ.option.get)
            } else if (typ.kind == TypeKind.Function) {
                resolve_function_type(typ.function.get)
            } else if (typ.kind == TypeKind.Unresolved) {
                resolve_unresolved_type(typ)
            } else {
                panic("resolve_type of kind " + string(typ.kind))
                false
            }
        }


        if (links.resolved == start) {

        } else {
            println("type resolved: " + string(links.resolved))
        }
        links.resolving = false
        links.resolved = resolved
        resolved
    }

    def get_type_of_symbol(symbol: Symbol): Type = {
        val links = get_symbol_links(symbol)
        if (links.has_type()) {
            links.get_type()
        } else if (symbol.has_declaration()) {
            val decl = symbol.declaration()
            val node_links = get_node_links(MakeNode.decl(decl))
            if (node_links.has_type()) {
                node_links.get_type()
            } else {
                if (links.typing) {
                    // we are already trying to type this symbol so we need to short ciruit
                    // once we get the type we should be able to reattempt resolving by
                    // updating the unresolved type
                    MakeType.unresolved(symbol)
                } else {
                    links.typing = true
                    val typ = get_type_of_declaration(decl, symbol.members)
                    links.typing = false
                    node_links.set_type(typ)
                    links.set_type(typ)
                    print("resolving type for " + symbol.name + "... ")
                    if(resolve_type(typ)) {
                        AstPrinter.printType(typ)
                        println()
                    } else {
                        AstPrinter.printColor(ColorPalette.Error)
                        println("failed")
                        AstPrinter.printClear()
                    }
                    typ
                }
            }
        } else {
            panic("get_type_of_symbol: no declaration")
            ErrorType
        }
    }

    def get_type_of_node(node: Node, scope: Scope): Type = {
        if (node.is_decl()) {
            get_type_of_declaration(node.declaration.get, scope)
        } else {
            get_type_of_expression(node.expression.get, scope)
        }
    }

    def get_type_of_declaration(decl: Declaration, scope: Scope): Type = {
        if (decl.kind == DeclarationKind.Object) {
            get_type_of_object_declaration(decl.object_declaration.get, scope)
        } else if (decl.kind == DeclarationKind.Class) {
            get_type_of_class_declaration(decl.class_declaration.get, scope)
        } else if (decl.kind == DeclarationKind.Method) {
            get_type_of_method_declaration(decl.function_declaration.get, scope)
        } else if (decl.kind == DeclarationKind.Parameter) {
            get_type_of_parameter_declaration(decl.parameter_declaration.get, scope)
        } else if (decl.kind == DeclarationKind.Local) {
            get_type_of_local_declaration(decl.local_declaration.get, scope)
        } else if (decl.kind == DeclarationKind.Token) {
            get_type_of_token(decl.token_declaration.get, scope)
        } else {
            panic("get_type_of_declaration")
            ErrorType
        }
    }

    def get_type_of_object_declaration(decl: ObjectDeclarationSyntax, scope: Scope): Type = {
        panic("get_type_of_object_declaration")
        ErrorType
    }

    def get_type_of_class_declaration(cls: ClassDeclarationSyntax, scope: Scope): Type = {
        val name = scope.name()
//        val fullName = if(name == "") cls.identifier.text else name + "." + cls.identifier.text

        MakeType.typeConstructor(scope.parent.symbol.get, new TypeConstructor(name, new Array[Type](0)))
    }

    def get_type_of_method_declaration(decl: FunctionDeclarationSyntax, scope: Scope): Type = {
        val param_types = new Array[Type](decl.parameters.length)
        for (i <- 0 to (decl.parameters.length-1)) {
            param_types(i) = get_type_of_parameter_declaration(decl.parameters(i), scope)
        }

        val returnType = if (decl.typeAnnotation.isDefined) {
            get_type_of_type_annotation(decl.typeAnnotation.get, scope)
        } else if (decl.body.isDefined) {
            get_type_of_expression(decl.body.get.expression, scope)
        } else {
            ErrorType
        }

        MakeType.function(scope.parent.symbol.get, param_types, returnType)
    }

    def get_type_of_parameter_declaration(decl: ParameterSyntax, scope: Scope): Type =
        get_type_of_type_annotation(decl.typeAnnotation, scope)

    def get_type_of_local_declaration(decl: VariableDeclarationStatementSyntax, scope: Scope): Type = {
        // check type annotation first
        if (decl.typeAnnotation.isDefined) {
            get_type_of_type_annotation(decl.typeAnnotation.get, scope)
        } else {
            get_type_of_expression(decl.expression, scope)
        }
    }

    def get_type_of_expression(expr: ExpressionSyntax, scope: Scope): Type = {
        val node = MakeNode.expr(expr)
        val links = get_node_links(node)
        if (links.has_type()) {
            links.get_type()
        } else {
            val typ = if (expr.kind == SyntaxKind.ArrayCreationExpression) {
                get_type_of_array_creation_expression(expr.arrayCreationExpression.get, scope)
            } else if (expr.kind == SyntaxKind.AssignmentExpression) {
                get_type_of_assignment_expression(expr.assignmentExpression.get)
            } else if (expr.kind == SyntaxKind.BinaryExpression) {
                get_type_of_binary_expression(expr.binaryExpression.get, scope)
            } else if (expr.kind == SyntaxKind.BlockExpression) {
                get_type_of_block_expression(expr.blockExpression.get, scope)
            } else if (expr.kind == SyntaxKind.CallExpression) {
                get_type_of_call_expression(expr.callExpression.get)
            } else if (expr.kind == SyntaxKind.ForExpression) {
                get_type_of_for_expression(expr.forExpression.get)
            } else if (expr.kind == SyntaxKind.GroupExpression) {
                get_type_of_group_expression(expr.groupExpression.get, scope)
            } else if (expr.kind == SyntaxKind.IdentifierName) {
                get_type_of_identifier_name(expr.identifierName.get, scope)
            } else if (expr.kind == SyntaxKind.IfExpression) {
                get_type_of_if_expression(expr.ifExpression.get, scope)
            } else if (expr.kind == SyntaxKind.IndexExpression) {
                get_type_of_index_expression(expr.indexExpression.get)
            } else if (expr.kind == SyntaxKind.LiteralExpression) {
                get_type_of_literal_expression(expr.literalExpression.get)
            } else if (expr.kind == SyntaxKind.MemberAccessExpression) {
                get_type_of_member_access_expression(expr.memberAccessExpression.get)
            } else if (expr.kind == SyntaxKind.NewExpression) {
                get_type_of_new_expression(expr.newExpression.get, scope)
            } else if (expr.kind == SyntaxKind.UnaryExpression) {
                get_type_of_unary_expression(expr.unaryExpression.get, scope)
            } else if (expr.kind == SyntaxKind.UnitExpression) {
                get_type_of_unit_expression(expr.unitExpression.get)
            } else if (expr.kind == SyntaxKind.WhileExpression) {
                get_type_of_while_expression(expr.whileExpression.get)
            } else {
                panic("get_type_of_expression: " + string(expr.kind))
                ErrorType
            }
            links.set_type(typ)
        }
    }

    def get_type_of_array_creation_expression(expr: ArrayCreationExpressionSyntax, scope: Scope): Type = {
        val inner = get_type_of_name(expr.name, scope)
        MakeType.array(get_type_of_name(expr.name, scope))
    }

    def get_type_of_assignment_expression(expr: AssignmentExpressionSyntax): Type = {
        panic("get_type_of_assignment_expression")
        ErrorType
    }
    def get_type_of_binary_expression(expr: BinaryExpressionSyntax, scope: Scope): Type = {
        val lhs = get_type_of_expression(expr.left, scope)
        val rhs = get_type_of_expression(expr.right, scope)
        panic("get_type_of_binary_expression")
        ErrorType
    }
    def get_type_of_block_expression(expr: BlockExpressionSyntax, scope: Scope): Type = {
        if (expr.expression.isDefined) get_type_of_expression(expr.expression.get, scope)
        else UnitType
    }
    def get_type_of_call_expression(expr: CallExpressionSyntax): Type = {
        panic("get_type_of_call_expression")
        ErrorType
    }
    def get_type_of_for_expression(expr: ForExpressionSyntax): Type = {
        panic("get_type_of_for_expression")
        ErrorType
    }
    def get_type_of_group_expression(expr: GroupExpressionSyntax, scope: Scope): Type =
        get_type_of_expression(expr.expression, scope)

    def get_type_of_identifier_name(expr: IdentifierNameSyntax, scope: Scope): Type =
        get_type_of_token(expr.identifier, scope)

    def get_type_of_if_expression(expr: IfExpressionSyntax, scope: Scope): Type = {
        val lhs = get_type_of_expression(expr.thenExpr, scope)
        val rhs = get_type_of_expression(expr.elseExpr, scope)
        if(lhs == rhs) lhs
        else {
            panic("get_type_of_if_expression")
            ErrorType
        }
    }
    def get_type_of_index_expression(expr: IndexExpressionSyntax): Type = {
        panic("get_type_of_index_expression")
        ErrorType
    }
    def get_type_of_literal_expression(expr: LiteralExpressionSyntax): Type = {
        if (expr.value.kind == SyntaxTokenValueKind.Boolean) BoolType
        else if (expr.value.kind == SyntaxTokenValueKind.String) StringType
        else if (expr.value.kind == SyntaxTokenValueKind.Character) CharType
        else if (expr.value.kind == SyntaxTokenValueKind.Number) IntType
        else {
            panic("get_type_of_literal_expression of kind: " + string(expr.value.kind))
            ErrorType
        }
    }

    def get_type_of_member_access_expression(expr: MemberAccessExpressionSyntax): Type = {
        panic("get_type_of_member_access_expression")
        ErrorType
    }

    def get_type_of_new_expression(expr: NewExpressionSyntax, scope: Scope): Type =
        get_type_of_name(expr.identifier, scope)

    def get_type_of_unary_expression(expr: UnaryExpressionSyntax, scope: Scope): Type = {
        val rhs = get_type_of_expression(expr.expression, scope)
        if (expr.operator.text == "-") {
            if (rhs.kind == TypeKind.Primitive && rhs.primitive.get.name == "int") {
                IntType
            } else if (rhs.kind == TypeKind.Primitive) {
                printLocation(expr.operator.location)
                panic("get_type_of_unary_expression: unknown primitive " + rhs.primitive.get.name)
                ErrorType
            } else {
                panic("get_type_of_unary_expression: unknown rhs.kind " + string(rhs.kind))
                ErrorType
            }
        } else {
            panic("get_type_of_unary_expression: unknown operator " + expr.operator.text)
            ErrorType
        }
    }

    def get_type_of_unit_expression(expr: UnitExpressionSyntax): Type = {
        panic("get_type_of_unit_expression")
        ErrorType
    }

    def get_type_of_while_expression(expr: WhileExpressionSyntax): Type = {
        panic("get_type_of_while_expression")
        ErrorType
    }


    def get_type_of_type_annotation(typ: TypeAnnotationSyntax, scope: Scope): Type =
        get_type_of_name(typ.typ, scope)

    def get_type_of_token(token: SyntaxToken, scope: Scope): Type =
        get_type_of_string(token.text, scope)

    def get_type_of_string(name: string, scope: Scope): Type = {
        if (name == "int") {
            IntType
        } else if (name == "char") {
            CharType
        } else if (name == "string") {
            StringType
        } else if (name == "bool") {
            BoolType
        } else if (name == "any") {
            AnyType
        } else if (name == "unit") {
            UnitType
        } else {
            val result = scope.get(name)
            if (result.isDefined) {
                get_type_of_symbol(result.get)
            } else {
                panic("get_type_of_string: " + name)
                ErrorType
            }
        }
    }

    def get_type_of_name(name: NameSyntax, scope: Scope): Type = {
        if (name.kind == SyntaxKind.SimpleName) {
            get_type_of_simple_name(name.simpleName.get, scope)
        } else if (name.kind == SyntaxKind.QualifiedName) {
            get_type_of_qualified_name(name.qualifiedName.get)
        } else {
            panic("get_type_of_name")
            ErrorType
        }
    }

    def get_type_of_simple_name(name: SimpleNameSyntax, scope: Scope): Type = {
        if (name.kind == SyntaxKind.IdentifierName) {
            get_type_of_token(name.identifierName.get.identifier, scope)
        } else if (name.kind == SyntaxKind.GenericName) {
            get_type_of_generic_name(name.genericName.get, scope)
        } else {
            panic("get_type_of_simple_name")
            ErrorType
        }
    }

    def get_type_of_generic_name(name: GenericNameSyntax, scope: Scope): Type = {
        if (name.identifier.text == "Array") {
            if (name.typeArgumentlist.arguments.length == 1) {
                val inner = get_type_of_name(name.typeArgumentlist.arguments(0).name, scope)
                MakeType.array(inner)
            } else {
                panic("Expected Array of rank 1, found rank " + string(name.typeArgumentlist.arguments.length))
                ErrorType
            }
        } else if (name.identifier.text == "Option") {
            if (name.typeArgumentlist.arguments.length == 1) {
                val inner = get_type_of_name(name.typeArgumentlist.arguments(0).name, scope)
                MakeType.option(inner)
            } else {
                panic("Expected Option with one type parameter, found " + string(name.typeArgumentlist.arguments.length))
                ErrorType
            }
        } else {
            panic("Expected Array, found: '" + name.identifier.text + "'")
            ErrorType
        }
    }

    def get_type_of_qualified_name(name: QualifiedNameSyntax): Type = {
        panic("get_type_of_qualified_name")
        ErrorType
    }

    def get_type_of_class_symbol(symbol: Symbol): Type = {
        panic("get_type_of_class_symbol")
        ErrorType
    }
    def get_type_of_method_symbol(symbol: Symbol): Type = {
        panic("get_type_of_method_symbol")
        ErrorType
    }
    def get_type_of_field_symbol(symbol: Symbol): Type = {
        panic("get_type_of_field_symbol")
        ErrorType
    }
    def get_type_of_parameter_symbol(symbol: Symbol): Type = {
        panic("get_type_of_parameter_symbol")
        ErrorType
    }
    def get_type_of_local_symbol(symbol: Symbol): Type = {
        panic("get_type_of_local_symbol")
        ErrorType
    }


    def get_node_id(node: Node): int = {
        if (node.id() != -1) {
            node.id()
        } else {
            if (last_node_id + 1 >= node_links.length) {
                val newItems = new Array[NodeLinks]((last_node_id + 1) * 2)
                for (i <- 0 to (last_node_id-1)) {
                    newItems(i) = node_links(i)
                }
                node_links = newItems
            } else { }

            node_links(last_node_id) = new NodeLinks()
            node.set_id(last_node_id)
            last_node_id = last_node_id + 1
            node.id()
        }
    }

    def get_node_links(node: Node): NodeLinks = {
        val id = get_node_id(node)
        node_links(id)
    }


    def get_type_id(typ: Type): int = {
        if (typ.id != -1) {
            typ.id
        } else {
            if (last_type_id + 1 >= type_links.length) {
                val newItems = new Array[TypeLinks]((last_type_id + 1) * 2)
                for (i <- 0 to (last_type_id-1)) {
                    newItems(i) = type_links(i)
                }
                type_links = newItems
            } else { }

            type_links(last_type_id) = new TypeLinks()
            typ.id = last_type_id
            last_type_id = last_type_id + 1
            typ.id
        }
    }

    def get_type_links(typ: Type): TypeLinks = {
        val id = get_type_id(typ)
        type_links(id)
    }

    def get_symbol_id(symbol: Symbol): int = {
        if (symbol.id != -1) {
            symbol.id
        } else {
            if (last_symbol_id + 1 >= symbol_links.length) {
                val newItems = new Array[SymbolLinks]((last_symbol_id + 1) * 2)
                for (i <- 0 to (last_symbol_id-1)) {
                    newItems(i) = symbol_links(i)
                }
                symbol_links = newItems
            } else { }

            symbol_links(last_symbol_id) = new SymbolLinks()
            symbol.id = last_symbol_id
            last_symbol_id = last_symbol_id + 1
            symbol.id
        }
    }

    def get_symbol_links(symbol: Symbol): SymbolLinks = {
        val id = get_symbol_id(symbol)
        symbol_links(id)
    }


    def printLocation(location: TextLocation): unit = {
        val span = location.span
        val source_file = location.source_file

        for (currrent_line <- location.start_line to location.end_line) {
            val line = source_file.get_line(currrent_line)
            val start_in_current = source_file.get_line_index(span.start) == currrent_line
            val end_in_current = source_file.get_line_index(span.end) == currrent_line

            val prefix_end =
                if (start_in_current) span.start
                else line.start

            val suffix_start =
                if (end_in_current) span.end
                else line.end

            val prefix_span = TextSpanFactory.from_bounds(line.start, prefix_end)
            val error_span = TextSpanFactory.from_bounds(prefix_end, suffix_start)
            val suffix_span = TextSpanFactory.from_bounds(suffix_start, line.end)

            val prefix = source_file.to_string(prefix_span)
            val error = source_file.to_string(error_span)
            val suffix = source_file.to_string(suffix_span)

            print(prefix)
            print(ANSI.foreground_color("e06c75"))
            print(error)
            print(ANSI.Clear)
            println(suffix)

            for(c <- 0 to (prefix_span.length - 1)) {
                print('-')
            }
            println('^')
        }

        println()
    }

    // // convert an inferred SimpleType into the immutable type representation
    // def coalesce_type(simple: SimpleType): Type =
    //     coalesce_type(simple, true)

    // def coalesce_type(simple: SimpleType, polarity: bool): Type = {
    //     if (simple.kind == SimpleTypeKind.Variable) {
    //         val tv = simple.variable[0]
    //         val bound = if (polarity) tv.lower_bound() else tv.upper_bound()
    //         val bound_type = coalesce_type(bound, polarity)
    //         if (polarity && bound.is_nothing() || bound.is_any()) tv.as_type_var()
    //         else if (polarity) MakeType.union(tv.as_type_var(), bound_type)
    //         else MakeType.intersection(tv.as_type_var(), bound_type)
    //     } else {
    //         val concrete = simple.concrete[0]
    //         if (concrete.kind == ConcreteTypeKind.Function) {
    //             MakeType.function(
    //                 concrete.function[0].symbol,
    //                 coalesce_types(concrete.function[0].params, !polarity),
    //                 coalesce_type(concrete.function[0].returnType, polarity)
    //             )
    //         } else if (concrete.kind == ConcreteTypeKind.Record) {
    //             MakeType.record(concrete.record[0].symbol, coalesce_record(concrete.record[0], polarity))
    //         } else if (concrete.kind == ConcreteTypeKind.Primitive) {
    //             MakeType.primitive(concrete.primitive[0].name)
    //         } else if (concrete.kind == ConcreteTypeKind.Any) {
    //             MakeType.top
    //         } else // if (concrete.kind == ConcreteTypeKind.Nothing) {
    //             MakeType.bottom
    //         }
    // }

    // def coalesce_types(simple_types: Array[SimpleType], polarity: bool): Array[Type] = {
    //     val types = new Type[simple_types.length]
    //     for (x <- 0 to simple_types.length) {
    //         types(x) = coalesce_type(simple_types(x), polarity)
    //     }
    //     types
    // }

    // def coalesce_record(record: Record, polarity: bool): RecordType = {
    //     val fields = new RecordTypeField[record.fields.length]
    //     for (x <- 0 to record.fields.length) {
    //         fields(x) = coalesce_record_field(record.fields(x), polarity)
    //     }
    //     new RecordType(fields)
    // }

    // def coalesce_record_field(field: RecordField, polarity: bool): RecordTypeField =
    //     new RecordTypeField(field.name, coalesce_type(field.type, polarity))
}