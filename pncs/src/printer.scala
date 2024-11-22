import panther._
import ExpressionSyntax._
import MemberSyntax._
import StatementSyntax._
import SimpleNameSyntax._
import NameSyntax._

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
    for (x <- 0 to (nodes.length - 1)) {
      printUsingDeclaration(nodes(x))
    }
  }

  def printMembers(nodes: Array[MemberSyntax]): unit = {
    for (x <- 0 to (nodes.length - 1)) {
      printMember(nodes(x))
    }
  }

  def printMember(member: MemberSyntax): unit = {
    member match {
      case value: ObjectDeclarationSyntax => printObjectDeclaration(value)
      case value: ClassDeclarationSyntax => printClassDeclaration(value)
      case value: FunctionDeclarationSyntax => printFunctionDeclaration(value)
      case value: GlobalStatementSyntax => printGlobalStatement(value)
      case value: EnumDeclarationSyntax => panic("EnumDeclarationSyntax not implemented")
    }
  }


  def printObjectDeclaration(node: MemberSyntax.ObjectDeclarationSyntax): unit = {
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

  def printClassDeclaration(node: MemberSyntax.ClassDeclarationSyntax): unit = {
    printToken(node.classKeyword)
    printToken(node.identifier, ColorPalette.Declarations)
    printToken(node.openParenToken)
    printParameters(node.parameters)
    printToken(node.closeParenToken)
    printOptionalTemplate(node.template)
  }

  def printFunctionDeclaration(node: MemberSyntax.FunctionDeclarationSyntax): unit = {
    printToken(node.defKeyword)
    printToken(node.identifier, ColorPalette.Members)
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
    node match {
      case ArrayCreationExpression(value) => printArrayCreationExpression(value)
      case AssignmentExpression(value) => printAssignmentExpression(value)
      case BinaryExpression(value) => printBinaryExpression(value)
      case BlockExpression(value) => printBlockExpression(value)
      case CallExpression(value) => printCallExpression(value)
      case ForExpression(value) => printForExpression(value)
      case GroupExpression(value) => printGroupExpression(value)
      case IdentifierName(value) => printIdentifierName(value, ColorPalette.Identifier)
      case IfExpression(value) => printIfExpression(value)
      case IndexExpression(value) => printIndexExpression(value)
      case LiteralExpression(value) => printLiteralExpression(value)
      case value: MatchExpression => panic("MatchExpression not implemented")
      case MemberAccessExpression(value) => printMemberAccessExpression(value)
      case NewExpression(value) => printNewExpression(value)
      case UnaryExpression(value) => printUnaryExpression(value)
      case UnitExpression(value) => printUnitExpression(value)
      case WhileExpression(value) => printWhileExpression(value)
    }
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
    printBlockExpressionList(node.block)
    printToken(node.closeBrace)
  }

  def printBlockExpressionList(node: BlockExpressionListSyntax): unit = {
    printStatments(node.statements)
    printOptionalExpression(node.expression)
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
    printName(node.name)
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
    printExpression(node.condition)
    printToken(node.closeParen)
    printExpression(node.body)
  }

  def printExpressionList(node: ExpressionListSyntax): unit = {
    printExpressionItems(node.expressions)
  }

  def printExpressionItems(nodes: Array[ExpressionItemSyntax]): unit = {
    for (x <- 0 to (nodes.length - 1)) {
      printExpressionItem(nodes(x))
    }
  }

  def printExpressionItem(node: ExpressionItemSyntax): unit = {
    printExpression(node.expression)
    printOptionalToken(node.separatorToken)
  }

  def printStatment(node: StatementSyntax): unit = {
    node match {
      case VariableDeclarationStatement(value) => printVariableDeclarationStatement(value)
      case BreakStatement(value) => printBreakStatement(value)
      case ContinueStatement(value) => printContinueStatement(value)
      case ExpressionStatement(value) => printExpressionStatement(value)
    }
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
    for (x <- 0 to (nodes.length - 1)) {
      printStatment(nodes(x))
    }
  }

  def printExpressions(nodes: Array[ExpressionSyntax]): unit = {
    for (x <- 0 to (nodes.length - 1)) {
      printExpression(nodes(x))
    }
  }

  def printParameters(nodes: Array[ParameterSyntax]): unit = {
    for (x <- 0 to (nodes.length - 1)) {
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
    name match {
      case value: NameSyntax.QualifiedNameSyntax => printQualifiedName(value)
      case value: NameSyntax.SimpleName => printSimpleName(value.name)
    }
  }

  def printQualifiedName(node: NameSyntax.QualifiedNameSyntax): unit = {
    printName(node.left)
    printToken(node.dotToken)
    printSimpleName(node.right)
  }

  def printGenericName(node: SimpleNameSyntax.GenericNameSyntax): unit = {
    printToken(node.identifier, ColorPalette.Declarations)
    printTypeArgumentList(node.typeArgumentlist)
  }

  def printSimpleName(node: SimpleNameSyntax): unit = {
    node match {
      case value: SimpleNameSyntax.GenericNameSyntax => printGenericName(value)
      case value: SimpleNameSyntax.IdentifierNameSyntax => printIdentifierName(value, ColorPalette.Declarations)
      case value: SimpleNameSyntax.ScalaAliasSyntax => printScalaAlias(value)
      case value: SimpleNameSyntax.AliasSyntax => printAlias(value)
    }
  }

  def printAlias(node: SimpleNameSyntax.AliasSyntax): unit = {
    printToken(node.name, ColorPalette.Identifier)
    printToken(node.asKeyword, ColorPalette.Punctuation)
    printToken(node.alias, ColorPalette.Identifier)
  }

  def printScalaAlias(node: SimpleNameSyntax.ScalaAliasSyntax): unit = {
    printToken(node.open, ColorPalette.Punctuation)
    printToken(node.name, ColorPalette.Identifier)
    printToken(node.arrow, ColorPalette.Punctuation)
    printToken(node.alias, ColorPalette.Identifier)
    printToken(node.close, ColorPalette.Punctuation)
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

  def printIdentifierNames(nodes: Array[SimpleNameSyntax.IdentifierNameSyntax], color: string): unit = {
    for (x <- 0 to (nodes.length - 1)) {
      printIdentifierName(nodes(x), color)
    }
  }

  def printIdentifierName(node: SimpleNameSyntax.IdentifierNameSyntax, color: string): unit = {
    printToken(node.identifier, color)
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
    typ match {
      case Type.Function(parameters, returnType) =>
        val lastIndex = parameters.length - 1
        printColor(ColorPalette.Punctuation)
        print("(")
        for (i <- 0 to (lastIndex)) {
          printColor(ColorPalette.Identifier)
          print(parameters(i).name)
          printColor(ColorPalette.Punctuation)
          print(": ")
          _printType(parameters(i).typ, false)
          if (i < lastIndex) {
            printColor(ColorPalette.Punctuation)
            print(", ")
          } else ()
        }
        printColor(ColorPalette.Punctuation)
        print(") -> ")
        _printType(returnType, false)
      case Type.Array(inner) =>
        printColor(ColorPalette.Keyword)
        print("Array")
        printColor(ColorPalette.Punctuation)
        print("<")
        _printType(inner, false)
        printColor(ColorPalette.Punctuation)
        print(">")

      case Type.Reference(symbol) =>
        printColor(ColorPalette.Identifier)
        print(symbol.name)
      case Type.Option(inner) =>
        printColor(ColorPalette.Keyword)
        print("Option")
        printColor(ColorPalette.Punctuation)
        print("<")
        _printType(inner, false)
        printColor(ColorPalette.Punctuation)
        print(">")
      case Type.Any =>
        printColor(ColorPalette.Keyword)
        print("any")
      case Type.Char =>
        printColor(ColorPalette.Keyword)
        print("char")
      case Type.Boolean =>
        printColor(ColorPalette.Keyword)
        print("bool")
      case Type.Int =>
        printColor(ColorPalette.Keyword)
        print("int")
      case Type.Never =>
        printColor(ColorPalette.Keyword)
        print("never")
      case Type.String =>
        printColor(ColorPalette.Keyword)
        print("string")
      case Type.Unit =>
        printColor(ColorPalette.Keyword)
        print("unit")
      case Type.Error =>
        printColor(ColorPalette.Error)
        print("error")
    }

    if (clear) printClear() else ()
  }

  def printKindColor(kind: int): unit = {
    if (kind == SyntaxKind.NumberToken || kind == SyntaxKind.TrueKeyword || kind == SyntaxKind.FalseKeyword) {
      printColor(ColorPalette.Literal1)
    } else if (SyntaxFacts.isKeywordKind(kind)) {
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
    println(token.location.to_string() + "[" + SyntaxFacts.getKindName(token.kind) + "]: " + "\"" + token.text + "\"")

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