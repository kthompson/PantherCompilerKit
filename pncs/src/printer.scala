import panther._

object ANSI {
    val Red = "\u001b[0;31m"
    val Clear = "\u001b[0m"


    // keyword c678dd
    // Identifier = e5c07b
    // whitespace = abb2bf
    // = 56b6c2
    // string #98c379

    def foreground_color(hex: string): string =
        foreground_color(Hex.from_string(hex, 0, 2), Hex.from_string(hex, 2, 2), Hex.from_string(hex, 4, 2))

    def foreground_color(r: int, g: int, b: int): string =
        "\u001b[38;2;" + string(r) + ";" + string(g) + ";" + string(b) + "m"

    def background_color(hex: string): string =
        background_color(Hex.from_string(hex, 0, 2), Hex.from_string(hex, 2, 2), Hex.from_string(hex, 4, 2))

    def background_color(r: int, g: int, b: int): string =
        "\u001b[48;2;" + string(r) + ";" + string(g) + ";" + string(b) + "m"
}

object ColorPalette {
    val Literal1 = "d19a66" // number / bool
    val Literal2 = "98c379" // string / char
    val Punctuation = "56b6c2"

    val Keyword = "c678dd" // if,else
    val Identifier = "e06c75"

    val Comments = "7f848e"
    val Declarations = "e5c07b"
    val Members = "61afef"

    val Error = "c73e1d"

    val Brackets = new Array[string](3)
    Brackets(0) = "d19a66"
    Brackets(1) = "c678dd"
    Brackets(2) = "56b6c2"
}

object AstPrinter {

    def printCompilationUnit(compilationUnit: CompilationUnitSyntax): unit = {
        printNamespaceDeclaration(compilationUnit.namespaceDeclaration)
        printUsingDeclarations(compilationUnit.usings)
        printMembers(compilationUnit.members)
        printToken(compilationUnit.endToken)
    }

    def printUsingDeclarations(nodes: Array[UsingDirectiveSyntax]): unit = {
        for (x <- 0 to (nodes.length-1)) {
            printUsingDeclaration(nodes(x))
        }
    }

    def printMembers(nodes: Array[MemberSyntax]): unit = {
        for (x <- 0 to (nodes.length-1)) {
            printMember(nodes(x))
        }
    }

    def printMember(member: MemberSyntax): unit = {
        val kind = member.kind
        if (kind == SyntaxKind.ObjectDeclaration) {
            printObjectDeclaration(member.objekt.get)
        } else if (kind == SyntaxKind.ClassDeclaration) {
            printClassDeclaration(member.klass.get)
        } else if (kind == SyntaxKind.FunctionDeclaration) {
            printFunctionDeclaration(member.function.get)
        } else if (kind == SyntaxKind.GlobalStatement) {
            printGlobalStatement(member.statement.get)
        } else panic("print_member")
    }


    def printObjectDeclaration(node: ObjectDeclarationSyntax): unit = {
        printToken(node.objectKeyword)
        printToken(node.identifier, ColorPalette.Declarations)
        printTemplate(node.template)
    }

    def printOptionalTemplate(node: Option[TemplateSyntax]): unit =
        if (node.isDefined) printTemplate(node.get) else ()

    def printTemplate(node: TemplateSyntax): unit = {
        printToken(node.openBrace)
        printMembers(node.members)
        printToken(node.closeBrace)
    }

    def printClassDeclaration(node: ClassDeclarationSyntax): unit ={
        printToken(node.classKeyword)
        printToken(node.identifier, ColorPalette.Declarations)
        printToken(node.openParenToken)
        printParameters(node.parameters)
        printToken(node.closeParenToken)
        printOptionalTemplate(node.template)
    }

    def printFunctionDeclaration(node: FunctionDeclarationSyntax): unit = {
        printToken(node.defKeyword)
        printToken(node.identifier, ColorPalette.Members)
        printToken(node.openParenToken)
        printParameters(node.parameters)
        printToken(node.closeParenToken)
        printOptionalTypeAnnotation(node.typeAnnotation)
        printOptionalFunctionBody(node.body)
    }

    def printGlobalStatement(node: GlobalStatementSyntax): unit =
        printStatment(node.statement)

    def printUsingDeclaration(node: UsingDirectiveSyntax): unit = {
        printToken(node.usingKeyword)
        printName(node.name)
    }

    def printParameter(node: ParameterSyntax): unit = {
        printToken(node.identifier)
        printTypeAnnotation(node.typeAnnotation)
        printOptionalToken(node.commaToken)
    }

    def printOptionalTypeAnnotation(node: Option[TypeAnnotationSyntax]): unit =
        if (node.isDefined) printTypeAnnotation(node.get) else ()

    def printTypeAnnotation(node: TypeAnnotationSyntax): unit = {
        printToken(node.colonToken)
        printName(node.typ)
    }

    def printOptionalFunctionBody(node: Option[FunctionBodySyntax]): unit =
        if (node.isDefined) printFunctionBody(node.get) else ()

    def printFunctionBody(node: FunctionBodySyntax): unit = {
        printToken(node.equalToken)
        printExpression(node.expression)
    }

    def printExpression(node: ExpressionSyntax): unit = {
        val kind = node.kind
        if (kind == SyntaxKind.ArrayCreationExpression) {
            printArrayCreationExpression(node.arrayCreationExpression.get)
        } else if (kind == SyntaxKind.AssignmentExpression) {
            printAssignmentExpression(node.assignmentExpression.get)
        } else if (kind == SyntaxKind.BinaryExpression) {
            printBinaryExpression(node.binaryExpression.get)
        } else if (kind == SyntaxKind.BlockExpression) {
            printBlockExpression(node.blockExpression.get)
        } else if (kind == SyntaxKind.CallExpression) {
            printCallExpression(node.callExpression.get)
        } else if (kind == SyntaxKind.ForExpression) {
            printForExpression(node.forExpression.get)
        } else if (kind == SyntaxKind.GroupExpression) {
            printGroupExpression(node.groupExpression.get)
        } else if (kind == SyntaxKind.IdentifierName) {
            printIdentifierName(node.identifierName.get, ColorPalette.Identifier)
        } else if (kind == SyntaxKind.IfExpression) {
            printIfExpression(node.ifExpression.get)
        } else if (kind == SyntaxKind.IndexExpression) {
            printIndexExpression(node.indexExpression.get)
        } else if (kind == SyntaxKind.LiteralExpression) {
            printLiteralExpression(node.literalExpression.get)
        } else if (kind == SyntaxKind.MemberAccessExpression) {
            printMemberAccessExpression(node.memberAccessExpression.get)
        } else if (kind == SyntaxKind.NewExpression) {
            printNewExpression(node.newExpression.get)
        } else if (kind == SyntaxKind.UnaryExpression) {
            printUnaryExpression(node.unaryExpression.get)
        } else if (kind == SyntaxKind.UnitExpression) {
            printUnitExpression(node.unitExpression.get)
        } else if (kind == SyntaxKind.WhileExpression) {
            printWhileExpression(node.whileExpression.get)
        } else ()
    }
    def printOptionalExpression(node: Option[ExpressionSyntax]): unit = {
        if (node.isDefined) {
            printExpression(node.get)
        } else {
            ()
        }
    }

    def printArrayCreationExpression(node: ArrayCreationExpressionSyntax): unit = {
        printToken(node.newKeyword)
        printName(node.name)
        printToken(node.openBracket)
        printOptionalExpression(node.arrayRank)
        printToken(node.closeBracket)
        printArrayInitializerExpressions(node.initializer)
    }

    def printArrayInitializerExpressions(node: Option[ArrayInitializerExpressionSyntax]): unit = {
        if (node.isDefined) {
            printArrayInitializerExpression(node.get)
        } else {
            ()
        }
    }
    def printArrayInitializerExpression(node: ArrayInitializerExpressionSyntax): unit = {
        printToken(node.openBrace)
        printExpressionList(node.expressions)
        printToken(node.closeBrace)
    }

    def printAssignmentExpression(node: AssignmentExpressionSyntax): unit = {
        printExpression(node.left)
        printToken(node.equals)
        printExpression(node.right)
    }
    def printBinaryExpression(node: BinaryExpressionSyntax): unit = {
        printExpression(node.left)
        printToken(node.operator)
        printExpression(node.right)
    }
    def printBlockExpression(node: BlockExpressionSyntax): unit = {
        printToken(node.openBrace)
        printStatments(node.statements)
        printOptionalExpression(node.expression)
        printToken(node.closeBrace)
    }

    def printCallExpression(node: CallExpressionSyntax): unit = {
        printExpression(node.name)
        printToken(node.openParen)
        printExpressionList(node.arguments)
        printToken(node.closeParen)
    }

    def printForExpression(node: ForExpressionSyntax): unit = {
        printToken(node.forKeyword)
        printToken(node.openParen)
        printToken(node.identifier)
        printToken(node.arrow)
        printExpression(node.fromExpr)
        printToken(node.toKeyword)
        printExpression(node.toExpr)
        printToken(node.closeParen)
        printExpression(node.body)
    }
    def printGroupExpression(node: GroupExpressionSyntax): unit = {
        printToken(node.openParen)
        printExpression(node.expression)
        printToken(node.closeParen)
    }
    def printIfExpression(node: IfExpressionSyntax): unit = {
        printToken(node.ifKeyword)
        printToken(node.openParen)
        printExpression(node.condition)
        printToken(node.closeParen)
        printExpression(node.thenExpr)
        printToken(node.elseKeyword)
        printExpression(node.elseExpr)
    }

    def printIndexExpression(node: IndexExpressionSyntax): unit = {
        printExpression(node.left)
        printToken(node.openBracket)
        printExpression(node.index)
        printToken(node.closeBracket)
    }
    def printLiteralExpression(node: LiteralExpressionSyntax): unit = {
        printToken(node.token)
    }
    def printMemberAccessExpression(node: MemberAccessExpressionSyntax): unit = {
        printExpression(node.left)
        printToken(node.dotToken)
        printIdentifierName(node.right, ColorPalette.Identifier)
    }
    def printNewExpression(node: NewExpressionSyntax): unit = {
        printToken(node.newKeyword)
        printName(node.identifier)
        printToken(node.openParen)
        printExpressionList(node.arguments)
        printToken(node.closeParen)
    }
    def printUnaryExpression(node: UnaryExpressionSyntax): unit = {
        printToken(node.operator)
        printExpression(node.expression)
    }
    def printUnitExpression(node: UnitExpressionSyntax): unit = {
        printToken(node.openParen)
        printToken(node.closeParen)
    }
    def printWhileExpression(node: WhileExpressionSyntax): unit = {
        printToken(node.whileKeyword)
        printToken(node.openParen)
        printExpression(node.left)
        printToken(node.closeParen)
        printExpression(node.body)
    }

    def printExpressionList(node: ExpressionListSyntax): unit = {
        printExpressionItems(node.expressions)
    }

    def printExpressionItems(nodes: Array[ExpressionItemSyntax]): unit = {
        for (x <- 0 to (nodes.length-1)) {
            printExpressionItem(nodes(x))
        }
    }

    def printExpressionItem(node: ExpressionItemSyntax): unit = {
        printExpression(node.expression)
        printOptionalToken(node.separatorToken)
    }

    def printStatment(node: StatementSyntax): unit = {
        val kind = node.kind
        if (kind == SyntaxKind.VariableDeclarationStatement) {
            printVariableDeclarationStatement(node.variableDeclarationStatement.get)
        } else if (kind == SyntaxKind.BreakStatement) {
            printBreakStatement(node.breakStatement.get)
        } else if (kind == SyntaxKind.ContinueStatement) {
            printContinueStatement(node.continueStatement.get)
        } else if (kind == SyntaxKind.ExpressionStatement) {
            printExpressionStatement(node.expressionStatement.get)
        } else ()
    }

    def printVariableDeclarationStatement(node: VariableDeclarationStatementSyntax): unit = {
        printToken(node.valOrVarKeyword)
        printToken(node.identifier)
        printOptionalTypeAnnotation(node.typeAnnotation)
        printToken(node.equalToken)
        printExpression(node.expression)
    }
    def printBreakStatement(node: BreakStatementSyntax): unit = printToken(node.breakKeyword)
    def printContinueStatement(node: ContinueStatementSyntax): unit = printToken(node.continueKeyword)
    def printExpressionStatement(node: ExpressionStatementSyntax): unit = {
        printExpression(node.expression)
    }

    def printStatments(nodes: Array[StatementSyntax]): unit = {
        for (x <- 0 to (nodes.length-1)) {
            printStatment(nodes(x))
        }
    }

    def printExpressions(nodes: Array[ExpressionSyntax]): unit = {
        for (x <- 0 to (nodes.length-1)) {
            printExpression(nodes(x))
        }
    }

    def printParameters(nodes: Array[ParameterSyntax]): unit = {
        for (x <- 0 to (nodes.length-1)) {
            printParameter(nodes(x))
        }
    }

    def printNamespaceDeclaration(decl: Option[NamespaceDeclarationSyntax]): unit = {
        if (decl.isDefined) {
            printToken(decl.get.namespaceKeyword)
            printName(decl.get.name)
        } else ()
    }

    def printName(name: NameSyntax): unit = {
        val kind = name.kind
        if (kind == SyntaxKind.QualifiedName) {
            printQualifiedName(name.qualifiedName.get)
        } else if (kind == SyntaxKind.SimpleName) {
            printSimpleName(name.simpleName.get)
        } else ()
    }

    def printQualifiedName(node: QualifiedNameSyntax): unit = {
        printName(node.left)
        printToken(node.dotToken)
        printSimpleName(node.right)
    }

    def printGenericName(node: GenericNameSyntax): unit = {
        printToken(node.identifier, ColorPalette.Declarations)
        printTypeArgumentList(node.typeArgumentlist)
    }

    def printSimpleName(node: SimpleNameSyntax): unit = {
        val kind = node.kind
        if (kind == SyntaxKind.GenericName) {
            printGenericName(node.genericName.get)
        } else if (kind == SyntaxKind.IdentifierName) {
            printIdentifierName(node.identifierName.get, ColorPalette.Declarations)
        } else ()
    }

    def printTypeArgumentLists(nodes: Array[TypeArgumentListSyntax]): unit = {
        for (x <- 0 to (nodes.length-1)) {
            printTypeArgumentList(nodes(x))
        }
    }

    def printTypeArgumentList(node: TypeArgumentListSyntax): unit = {
        printToken(node.lessThanToken)
        printTypeArgumentListItems(node.arguments)
        printToken(node.greaterThanToken)
    }

    def printTypeArgumentListItems(nodes: Array[TypeArgumentItemSyntax]): unit = {
        for (x <- 0 to (nodes.length-1)) {
            printTypeArgumentListItem(nodes(x))
        }
    }

    def printTypeArgumentListItem(node: TypeArgumentItemSyntax): unit = {
        printName(node.name)
        printOptionalToken(node.separator)
    }

    def printIdentifierNames(nodes: Array[IdentifierNameSyntax], color: string): unit = {
        for (x <- 0 to (nodes.length-1)) {
            printIdentifierName(nodes(x), color)
        }
    }

    def printIdentifierName(node: IdentifierNameSyntax, color: string): unit = {
        printToken(node.identifier, color)
    }

    def printTriviaArray(nodes: Array[SyntaxTrivia]): unit = {
        for (x <- 0 to (nodes.length-1)) {
            printTrivia(nodes(x))
        }
    }
    def printTrivia(node: SyntaxTrivia): unit = {
        printKindColor(node.kind)
        print(node.text)
        printClear()
    }

    def printTokens(nodes: Array[SyntaxToken]): unit = {
        for (x <- 0 to (nodes.length-1)) {
            printToken(nodes(x))
        }
    }

    def printOptionalToken(node: Option[SyntaxToken]): unit = {
        if (node.isDefined) {
            printToken(node.get)
        } else ()
    }

    def printToken(token: SyntaxToken): unit = {
        printTriviaArray(token.leading)
        printKindColor(token.kind)
        print(token.text)
        printClear()
        printTriviaArray(token.trailing)
    }

    def printToken(token: SyntaxToken, color: string): unit = {
        printTriviaArray(token.leading)
        printColor(color)
        print(token.text)
        printClear()
        printTriviaArray(token.trailing)
    }

    def printType(typ: Type): unit =
        _printType(typ, true)


    def _printType(typ: Type, clear: bool): unit = {
        if (typ.kind == TypeKind.Primitive) {
            printColor(ColorPalette.Keyword)
            print(typ.primitive.get.name)
        } else if (typ.kind == TypeKind.TypeConstructor) {
            printColor(ColorPalette.Identifier)
            print(typ.typeConstructor.get.name)
        } else if (typ.kind == TypeKind.Array) {
            printColor(ColorPalette.Identifier)
            print("Array")
            printColor(ColorPalette.Punctuation)
            print("<")
            _printType(typ.array.get.inner, false)
            printColor(ColorPalette.Punctuation)
            print(">")
        } else if (typ.kind == TypeKind.Option) {
            printColor(ColorPalette.Identifier)
            print("Option")
            printColor(ColorPalette.Punctuation)
            print("<")
            _printType(typ.option.get.inner, false)
            printColor(ColorPalette.Punctuation)
            print(">")
        } else if (typ.kind == TypeKind.Function) {
            for(i <- 0 to (typ.function.get.parameters.length-1)) {
                _printType(typ.function.get.parameters(i), false)
                if(i < typ.function.get.parameters.length - 1) {
                    printColor(ColorPalette.Punctuation)
                    print(", ")
                } else ()
            }
            printColor(ColorPalette.Punctuation)
            print(" -> ")
            _printType(typ.function.get.returnType, false)
        } else {
            panic("getTypeName: " + typ.kind)
        }

        if(clear) printClear() else ()
    }

    def printKindColor(kind: int): unit = {
        if (kind == SyntaxKind.NumberToken || kind == SyntaxKind.TrueKeyword || kind == SyntaxKind.FalseKeyword) {
            printColor(ColorPalette.Literal1)
        } else if (SyntaxFacts.is_keyword_kind(kind)) {
            printColor(ColorPalette.Keyword)
        } else if (kind == SyntaxKind.StringToken || kind == SyntaxKind.CharToken) {
            printColor(ColorPalette.Literal2)
        } else if (kind == SyntaxKind.DashToken) {
            printColor(ColorPalette.Punctuation)
        } else if (kind == SyntaxKind.LineCommentTrivia || kind == SyntaxKind.BlockCommentTrivia) {
            printColor(ColorPalette.Comments)
        } else if (kind == SyntaxKind.IdentifierToken) {
            printColor(ColorPalette.Identifier)
        } else if (kind == SyntaxKind.OpenBraceToken || kind == SyntaxKind.OpenParenToken || kind == SyntaxKind.OpenBracketToken ||
                   kind == SyntaxKind.CloseBraceToken || kind == SyntaxKind.CloseParenToken || kind == SyntaxKind.CloseBracketToken) {
            printBracketColor(kind)
        } else {

        }
    }

    var bracketDepth = 0

    def updateBracketDepth(value: int): unit = {
        bracketDepth = value

        while (bracketDepth >= 3) {
            bracketDepth = bracketDepth - 3
        }
        while (bracketDepth < 0) {
            bracketDepth = bracketDepth + 3
        }
    }

    def printBracketColor(kind: int): unit = {
        if (kind == SyntaxKind.OpenBraceToken || kind == SyntaxKind.OpenParenToken || kind == SyntaxKind.OpenBracketToken) {
            printColor(ColorPalette.Brackets(bracketDepth))
            updateBracketDepth(bracketDepth + 1)
        } else {
            updateBracketDepth(bracketDepth - 1)
            printColor(ColorPalette.Brackets(bracketDepth))
        }
    }

    def printTokenInfo(token: SyntaxToken): unit =
        println(token.location.to_string() + "[" + SyntaxFacts.get_kind_name(token.kind) + "]: " + "\"" + token.text + "\"")

    def padRight(value: string, len: int): string = {
        var padded = value
        while (padded.length < len) {
            padded = padded + " "
        }

        padded
    }

    def printColor(value: string): unit = print(ANSI.foreground_color(value))
    def printClear(): unit = print(ANSI.Clear)
}