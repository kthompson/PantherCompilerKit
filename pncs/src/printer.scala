import panther._
import Expression._
import MemberSyntax._
import StatementSyntax._
import SimpleNameSyntax._
import NameSyntax._

object AstPrinter {

  def printCompilationUnit(compilationUnit: CompilationUnitSyntax): unit = {
    printNamespaceDeclaration(compilationUnit.namespaceDeclaration)
    printUsingDeclarations(compilationUnit.usings)
    printMembers(compilationUnit.members)
    printToken(compilationUnit.endToken)
  }

  def printUsingDeclarations(nodes: Array[UsingDirectiveSyntax]): unit = {
    for (x <- 0 to (nodes.length - 1)) {
      printUsingDeclaration(nodes(x))
    }
  }

  def printMembers(nodes: List[MemberSyntax]): unit = {
    nodes match {
      case List.Nil => ()
      case List.Cons(x, xs) =>
        printMember(x)
        printMembers(xs)
    }
  }

  def printMember(member: MemberSyntax): unit = {
    member match {
      case value: ObjectDeclarationSyntax   => printObjectDeclaration(value)
      case value: ClassDeclarationSyntax    => printClassDeclaration(value)
      case value: FunctionDeclarationSyntax => printFunctionDeclaration(value)
      case value: GlobalStatementSyntax     => printGlobalStatement(value)
      case value: VariableDeclaration =>
        panic("VariableDeclaration not implemented")
      case value: EnumDeclarationSyntax =>
        panic("EnumDeclarationSyntax not implemented")
    }
  }

  def printObjectDeclaration(
      node: MemberSyntax.ObjectDeclarationSyntax
  ): unit = {
    printTokenWithColor(node.objectKeyword, ColorPalette.Keyword)
    printTokenWithColor(node.identifier, ColorPalette.Identifier)
    printTemplate(node.template)
  }

  def printOptionalTemplate(node: Option[TemplateSyntax]): unit =
    node match {
      case Option.Some(value) => printTemplate(value)
      case Option.None        =>
    }

  def printTemplate(node: TemplateSyntax): unit = {
    printToken(node.openBrace)
    printMembers(node.members)
    printToken(node.closeBrace)
  }

  def printClassDeclaration(node: MemberSyntax.ClassDeclarationSyntax): unit = {
    printTokenWithColor(node.classKeyword, ColorPalette.Keyword)
    printTokenWithColor(node.identifier, ColorPalette.Identifier)
    node.genericParameters match {
      case Option.None        => ()
      case Option.Some(value) => printGenericParameters(value)
    }
    printToken(node.openParenToken)
    printParameters(node.parameters)
    printToken(node.closeParenToken)
    printOptionalTemplate(node.template)
  }

  def printGenericParameters(
      node: GenericParametersSyntax
  ): unit = {
    printTokenWithColor(node.lessThanToken, ColorPalette.Brackets(0))
    printGenericTypeParameterList(
      node.parameters.items,
      node.parameters.separators
    )
    printTokenWithColor(node.greaterThanToken, ColorPalette.Brackets(0))
  }

  def printGenericTypeParameterList(
      items: List[GenericParameterSyntax],
      separators: List[SyntaxToken]
  ): unit = {
    items match {
      case List.Nil => ()
      case List.Cons(genParam, parameterTail) =>
        printGenericTypeParameter(genParam)
        separators match {
          case List.Nil =>
            assert(
              parameterTail.isEmpty,
              "Expected no more generic parameters"
            )

          case List.Cons(head, separatorTail) =>
            printTokenWithColor(head, ColorPalette.Punctuation)
            printGenericTypeParameterList(parameterTail, separatorTail)
        }
    }
  }

  def printGenericTypeParameter(node: GenericParameterSyntax): unit = {
    node.variance match {
      case Option.None => ()
      case Option.Some(value) =>
        printTokenWithColor(value, ColorPalette.TypeParameter)
    }

    printTokenWithColor(node.identifier, ColorPalette.TypeParameter)
    node.bounds match {
      case Option.None => ()
      case Option.Some(value) =>
        printTokenWithColor(value.token, ColorPalette.Punctuation)
        printName(value.name)
    }
  }

  def printFunctionDeclaration(
      node: MemberSyntax.FunctionDeclarationSyntax
  ): unit = {
    printTokenWithColor(node.defKeyword, ColorPalette.Keyword)
    printTokenWithColor(node.identifier, ColorPalette.Method)
    printToken(node.openParenToken)
    printParameters(node.parameters)
    printToken(node.closeParenToken)
    printOptionalTypeAnnotation(node.typeAnnotation)
    printOptionalFunctionBody(node.body)
  }

  def printGlobalStatement(node: MemberSyntax.GlobalStatementSyntax): unit =
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
    if (node.isDefined()) printTypeAnnotation(node.get()) else ()

  def printTypeAnnotation(node: TypeAnnotationSyntax): unit = {
    printToken(node.colonToken)
    printName(node.typ)
  }

  def printOptionalFunctionBody(node: Option[FunctionBodySyntax]): unit =
    if (node.isDefined()) printFunctionBody(node.get()) else ()

  def printFunctionBody(node: FunctionBodySyntax): unit = {
    printToken(node.equalToken)
    printExpression(node.expression)
  }

  def printExpression(node: Expression): unit = {
    node match {
      case value: Expression.ArrayCreationExpression =>
        printArrayCreationExpression(value)
      case value: Expression.AssignmentExpression =>
        printAssignmentExpression(value)
      case value: Expression.BinaryExpression => printBinaryExpression(value)
      case value: Expression.BlockExpression  => printBlockExpression(value)
      case value: Expression.CallExpression   => printCallExpression(value)
      case value: Expression.ForExpression    => printForExpression(value)
      case value: Expression.GroupExpression  => printGroupExpression(value)
      case IdentifierName(value) =>
        printIdentifierName(value, ColorPalette.Identifier)
      case value: Expression.If                => printIfExpression(value)
      case value: Expression.IndexExpression   => printIndexExpression(value)
      case value: Expression.LiteralExpression => printLiteralExpression(value)
      case value: MatchExpression => panic("MatchExpression not implemented")
      case value: Expression.MemberAccessExpression =>
        printMemberAccessExpression(value)
      case value: Expression.NewExpression   => printNewExpression(value)
      case value: Expression.UnaryExpression => printUnaryExpression(value)
      case value: Expression.UnitExpression  => printUnitExpression(value)
      case value: Expression.WhileExpression => printWhileExpression(value)
    }
  }

  def printOptionalExpression(node: Option[Expression]): unit = {
    if (node.isDefined()) {
      printExpression(node.get())
    }
  }

  def printArrayCreationExpression(
      node: Expression.ArrayCreationExpression
  ): unit = {
    printToken(node.newKeyword)
    printName(node.name)
    printToken(node.openBracket)
    printOptionalExpression(node.arrayRank)
    printToken(node.closeBracket)
    printArrayInitializerExpressions(node.initializer)
  }

  def printArrayInitializerExpressions(
      node: Option[ArrayInitializerExpressionSyntax]
  ): unit = {
    if (node.isDefined()) {
      printArrayInitializerExpression(node.get())
    }
  }

  def printArrayInitializerExpression(
      node: ArrayInitializerExpressionSyntax
  ): unit = {
    printToken(node.openBrace)
    printExpressionList(node.expressions)
    printToken(node.closeBrace)
  }

  def printAssignmentExpression(node: Expression.AssignmentExpression): unit = {
    printExpression(node.left)
    printToken(node.equals)
    printExpression(node.right)
  }

  def printBinaryExpression(node: Expression.BinaryExpression): unit = {
    printExpression(node.left)
    printToken(node.operator)
    printExpression(node.right)
  }

  def printBlockExpression(node: Expression.BlockExpression): unit = {
    printToken(node.openBrace)
    printBlockExpressionList(node.block)
    printToken(node.closeBrace)
  }

  def printBlockExpressionList(node: BlockExpressionListSyntax): unit = {
    printStatments(node.statements)
    printOptionalExpression(node.expression)
  }

  def printCallExpression(node: Expression.CallExpression): unit = {
    printExpression(node.name)
    printToken(node.openParen)
    printExpressionList(node.arguments)
    printToken(node.closeParen)
  }

  def printForExpression(node: Expression.ForExpression): unit = {
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

  def printGroupExpression(node: Expression.GroupExpression): unit = {
    printToken(node.openParen)
    printExpression(node.expression)
    printToken(node.closeParen)
  }

  def printIfExpression(node: Expression.If): unit = {
    printToken(node.ifKeyword)
    printToken(node.openParen)
    printExpression(node.condition)
    printToken(node.closeParen)
    printExpression(node.thenExpr)
    printElseExpression(node.elseExpr)
  }

  def printElseExpression(node: Option[ElseSyntax]): unit = {
    node match {
      case Option.Some(value) =>
        printToken(value.elseKeyword)
        printExpression(value.expression)
      case Option.None => ()
    }
  }

  def printIndexExpression(node: Expression.IndexExpression): unit = {
    printExpression(node.left)
    printToken(node.openBracket)
    printExpression(node.index)
    printToken(node.closeBracket)
  }

  def printLiteralExpression(node: Expression.LiteralExpression): unit = {
    printToken(node.token)
  }

  def printMemberAccessExpression(
      node: Expression.MemberAccessExpression
  ): unit = {
    printExpression(node.left)
    printToken(node.dotToken)
    printIdentifierName(node.right, ColorPalette.Identifier)
  }

  def printNewExpression(node: Expression.NewExpression): unit = {
    printToken(node.newKeyword)
    printName(node.name)
    printToken(node.openParen)
    printExpressionList(node.arguments)
    printToken(node.closeParen)
  }

  def printUnaryExpression(node: Expression.UnaryExpression): unit = {
    printToken(node.operator)
    printExpression(node.expression)
  }

  def printUnitExpression(node: Expression.UnitExpression): unit = {
    printToken(node.openParen)
    printToken(node.closeParen)
  }

  def printWhileExpression(node: Expression.WhileExpression): unit = {
    printToken(node.whileKeyword)
    printToken(node.openParen)
    printExpression(node.condition)
    printToken(node.closeParen)
    printExpression(node.body)
  }

  def printExpressionList(node: ExpressionListSyntax): unit = {
    printExpressionItems(node.expressions)
  }

  def printExpressionItems(nodes: List[ExpressionItemSyntax]): unit = {
    nodes match {
      case List.Nil => ()
      case List.Cons(x, xs) =>
        printExpressionItem(x)
        printExpressionItems(xs)
    }
  }

  def printExpressionItem(node: ExpressionItemSyntax): unit = {
    printExpression(node.expression)
    printOptionalToken(node.separatorToken)
  }

  def printStatment(node: StatementSyntax): unit = {
    node match {
      case value: VariableDeclarationStatement =>
        printVariableDeclarationStatement(value)
      case value: BreakStatement      => printBreakStatement(value)
      case value: ContinueStatement   => printContinueStatement(value)
      case value: ExpressionStatement => printExpressionStatement(value)
    }
  }

  def printVariableDeclarationStatement(
      node: StatementSyntax.VariableDeclarationStatement
  ): unit = {
    printToken(node.valOrVarKeyword)
    printToken(node.identifier)
    printOptionalTypeAnnotation(node.typeAnnotation)
    printToken(node.equalToken)
    printExpression(node.expression)
  }

  def printBreakStatement(node: StatementSyntax.BreakStatement): unit =
    printToken(node.breakKeyword)

  def printContinueStatement(node: StatementSyntax.ContinueStatement): unit =
    printToken(node.continueKeyword)

  def printExpressionStatement(
      node: StatementSyntax.ExpressionStatement
  ): unit = {
    printExpression(node.expression)
  }

  def printStatments(nodes: List[StatementSyntax]): unit = {
    nodes match {
      case List.Nil => ()
      case List.Cons(x, xs) =>
        printStatment(x)
        printStatments(xs)
    }
  }

  def printExpressions(nodes: Array[Expression]): unit = {
    for (x <- 0 to (nodes.length - 1)) {
      printExpression(nodes(x))
    }
  }

  def printParameters(nodes: List[ParameterSyntax]): unit = {
    nodes match {
      case List.Nil =>
      case List.Cons(head, tail) =>
        printParameter(head)
        printParameters(tail)
    }
  }

  def printNamespaceDeclaration(
      decl: Option[NamespaceDeclarationSyntax]
  ): unit = {
    if (decl.isDefined()) {
      printToken(decl.get().namespaceKeyword)
      printName(decl.get().name)
    } else ()
  }

  def printName(name: NameSyntax): unit = {
    name match {
      case value: NameSyntax.QualifiedName => printQualifiedName(value)
      case value: NameSyntax.SimpleName    => printSimpleName(value.name)
    }
  }

  def printQualifiedName(node: NameSyntax.QualifiedName): unit = {
    printName(node.left)
    printToken(node.dotToken)
    printSimpleName(node.right)
  }

  def printGenericName(node: SimpleNameSyntax.GenericNameSyntax): unit = {
    printTokenWithColor(node.identifier, ColorPalette.Identifier)
    printTypeArgumentList(node.typeArgumentlist)
  }

  def printSimpleName(node: SimpleNameSyntax): unit = {
    node match {
      case value: SimpleNameSyntax.GenericNameSyntax => printGenericName(value)
      case value: SimpleNameSyntax.IdentifierNameSyntax =>
        printIdentifierName(value, ColorPalette.Identifier)
      case value: SimpleNameSyntax.ScalaAliasSyntax => printScalaAlias(value)
      case value: SimpleNameSyntax.AliasSyntax      => printAlias(value)
    }
  }

  def printAlias(node: SimpleNameSyntax.AliasSyntax): unit = {
    printTokenWithColor(node.name, ColorPalette.Identifier)
    printTokenWithColor(node.asKeyword, ColorPalette.Punctuation)
    printTokenWithColor(node.alias, ColorPalette.Identifier)
  }

  def printScalaAlias(node: SimpleNameSyntax.ScalaAliasSyntax): unit = {
    printTokenWithColor(node.open, ColorPalette.Punctuation)
    printTokenWithColor(node.name, ColorPalette.Identifier)
    printTokenWithColor(node.arrow, ColorPalette.Punctuation)
    printTokenWithColor(node.alias, ColorPalette.Identifier)
    printTokenWithColor(node.close, ColorPalette.Punctuation)
  }

  def printTypeArgumentLists(nodes: Array[TypeArgumentListSyntax]): unit = {
    for (x <- 0 to (nodes.length - 1)) {
      printTypeArgumentList(nodes(x))
    }
  }

  def printTypeArgumentList(node: TypeArgumentListSyntax): unit = {
    printToken(node.lessThanToken)
    printTypeArgumentListItems(node.arguments)
    printToken(node.greaterThanToken)
  }

  def printTypeArgumentListItems(nodes: Array[TypeArgumentItemSyntax]): unit = {
    for (x <- 0 to (nodes.length - 1)) {
      printTypeArgumentListItem(nodes(x))
    }
  }

  def printTypeArgumentListItem(node: TypeArgumentItemSyntax): unit = {
    printName(node.name)
    printOptionalToken(node.separator)
  }

  def printIdentifierNames(
      nodes: Array[SimpleNameSyntax.IdentifierNameSyntax],
      color: string
  ): unit = {
    for (x <- 0 to (nodes.length - 1)) {
      printIdentifierName(nodes(x), color)
    }
  }

  def printIdentifierName(
      node: SimpleNameSyntax.IdentifierNameSyntax,
      color: string
  ): unit = {
    printTokenWithColor(node.identifier, color)
  }

  def printTriviaArray(nodes: Array[SyntaxTrivia]): unit = {
    for (x <- 0 to (nodes.length - 1)) {
      printTrivia(nodes(x))
    }
  }

  def printTrivia(node: SyntaxTrivia): unit = {
    printKindColor(node.kind)
    print(node.text)
    printClear()
  }

  def printTokens(nodes: Array[SyntaxToken]): unit = {
    for (x <- 0 to (nodes.length - 1)) {
      printToken(nodes(x))
    }
  }

  def printOptionalToken(node: Option[SyntaxToken]): unit = {
    if (node.isDefined()) {
      printToken(node.get())
    } else ()
  }

  def printToken(token: SyntaxToken): unit = {
    printTriviaArray(token.leading)
    printKindColor(token.kind)
    print(token.text)
    printClear()
    printTriviaArray(token.trailing)
  }

  def printTokenWithColor(token: SyntaxToken, color: string): unit = {
    printTriviaArray(token.leading)
    printColor(color)
    print(token.text)
    printClear()
    printTriviaArray(token.trailing)
  }

  def printType(typ: Type): unit =
    _printType(typ, true)

  def _printTypes(types: List[Type], first: bool): unit =
    types match {
      case List.Nil => printClear()
      case List.Cons(typ, tail) =>
        if (!first) {
          printColor(ColorPalette.Punctuation)
          print(", ")
        }
        _printType(typ, false)
        _printTypes(tail, false)
    }

  def _printTypedParameters(
      printedFirst: bool,
      parameters: List[BoundParameter]
  ): unit = {
    parameters match {
      case List.Nil => ()
      case List.Cons(param, tail) =>
        if (printedFirst) {
          printColor(ColorPalette.Punctuation)
          print(", ")
        } else {}
        printColor(ColorPalette.Identifier)
        print(param.name)
        printColor(ColorPalette.Punctuation)
        print(": ")
        _printType(param.typ, false)
        _printTypedParameters(true, tail)
    }
  }

  def _printType(typ: Type, clear: bool): unit = {
    typ match {
      case Type.Function(_, parameters, returnType) =>
        printColor(ColorPalette.Punctuation)
        print("(")
        _printTypedParameters(false, parameters)
        printColor(ColorPalette.Punctuation)
        print(") -> ")
        _printType(returnType, false)
      case Type.GenericFunction(_, genParams, traits, parameters, returnType) =>
        printColor(ColorPalette.Punctuation)
        print("<")
        _printGenTypes(genParams, true)
        printColor(ColorPalette.Punctuation)
        print(">")
        print("(")
        _printTypedParameters(false, parameters)
        printColor(ColorPalette.Punctuation)
        print(") -> ")
        _printType(returnType, false)

      case Type.Class(_, ns, name, args) =>
        val color =
          if (ns.isEmpty && SyntaxFacts.isBuiltinType(name))
            ColorPalette.Keyword
          else ColorPalette.Identifier

        printColor(color)
        print(typ._name(ns, name))

        args match {
          case List.Nil =>
          case List.Cons(typ, tail) =>
            print("<")
            _printTypes(args, true)
            print(">")
        }

      case Type.GenericClass(_, ns, name, args) =>
        val color =
          if (ns.isEmpty && SyntaxFacts.isBuiltinType(name))
            ColorPalette.Keyword
          else ColorPalette.Identifier

        printColor(color)
        print(typ._name(ns, name))

        args match {
          case List.Nil =>
          case List.Cons(typ, tail) =>
            print("<")
            _printGenTypes(args, true)
            print(">")
        }
      case Type.Variable(_, i) =>
        printColor(ColorPalette.Keyword)
        print("$" + i)
      case Type.Any =>
        printColor(ColorPalette.Keyword)
        print("any")
      case Type.Never =>
        printColor(ColorPalette.Keyword)
        print("never")
      case Type.Error =>
        printColor(ColorPalette.Error)
        print("error")
    }

    if (clear) printClear() else ()
  }

  def _printGenTypes(types: List[GenericTypeParameter], first: bool): unit = {
    types match {
      case List.Nil => printClear()
      case List.Cons(typ, tail) =>
        if (!first) {
          printColor(ColorPalette.Punctuation)
          print(", ")
        }

        typ.variance match {
          case Variance.Covariant =>
            printColor(ColorPalette.Keyword)
            print("out ")
          case Variance.Contravariant =>
            printColor(ColorPalette.Keyword)
            print("in ")
          case _ =>
        }

        printColor(ColorPalette.Identifier)
        print(typ.name)
        typ.upperBound match {
          case Option.Some(value) =>
            printColor(ColorPalette.Punctuation)
            print(" <: ")
            _printType(value, false)
          case Option.None =>
        }

        _printGenTypes(tail, false)
    }
  }

  def printSymbolKind(kind: SymbolKind): unit = {
    kind match {
      case SymbolKind.Namespace =>
        printColor(ColorPalette.Keyword)
        print("Namespace")
      case SymbolKind.Class =>
        printColor(ColorPalette.Keyword)
        print("Class")
      case SymbolKind.Object =>
        printColor(ColorPalette.Keyword)
        print("Object")
      case SymbolKind.Enum =>
        printColor(ColorPalette.Keyword)
        print("Enum")
      case SymbolKind.TypeParameter(_) =>
        printColor(ColorPalette.TypeParameter)
        print("TypeParameter")
      case SymbolKind.Field =>
        printColor(ColorPalette.Field)
        print("Field")
      case SymbolKind.Method =>
        printColor(ColorPalette.Method)
        print("Method")
      case SymbolKind.Constructor =>
        printColor(ColorPalette.Method)
        print("Constructor")
      case SymbolKind.Block =>
        printColor(ColorPalette.Identifier)
        print("Block")
      case SymbolKind.Parameter =>
        printColor(ColorPalette.Identifier)
        print("Parameter")
      case SymbolKind.Local =>
        printColor(ColorPalette.Identifier)
        print("Local")
    }

    print(ANSI.Clear)
  }

  def printKindColor(kind: int): unit = {
    if (kind == SyntaxKind.NumberToken) {
      printColor(ColorPalette.Number)
    } else if (SyntaxFacts.isKeywordKind(kind)) {
      printColor(ColorPalette.Keyword)
    } else if (kind == SyntaxKind.StringToken || kind == SyntaxKind.CharToken) {
      printColor(ColorPalette.String)
    } else if (kind == SyntaxKind.DashToken) {
      printColor(ColorPalette.Punctuation)
    } else if (
      kind == SyntaxKind.LineCommentTrivia || kind == SyntaxKind.BlockCommentTrivia
    ) {
      printColor(ColorPalette.Comment)
    } else if (kind == SyntaxKind.IdentifierToken) {
      printColor(ColorPalette.Identifier)
    } else if (
      kind == SyntaxKind.OpenBraceToken || kind == SyntaxKind.OpenParenToken || kind == SyntaxKind.OpenBracketToken ||
      kind == SyntaxKind.CloseBraceToken || kind == SyntaxKind.CloseParenToken || kind == SyntaxKind.CloseBracketToken
    ) {
      printBracketColor(kind)
    } else {}
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
    if (
      kind == SyntaxKind.OpenBraceToken || kind == SyntaxKind.OpenParenToken || kind == SyntaxKind.OpenBracketToken
    ) {
      printColor(ColorPalette.Brackets(bracketDepth))
      updateBracketDepth(bracketDepth + 1)
    } else {
      updateBracketDepth(bracketDepth - 1)
      printColor(ColorPalette.Brackets(bracketDepth))
    }
  }

  def printTokenInfo(token: SyntaxToken): unit =
    println(
      token.location.toString() + "[" + SyntaxFacts.getKindName(
        token.kind
      ) + "]: " + "\"" + token.text + "\""
    )

  def padRight(value: string, len: int): string = {
    var padded = value
    while (padded.length < len) {
      padded = padded + " "
    }

    padded
  }

  def printColor(value: string): unit = print(ANSI.foregroundColor(value))

  def printClear(): unit = print(ANSI.Clear)
}
