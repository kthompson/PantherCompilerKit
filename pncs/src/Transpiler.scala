import panther._
import system.io._

case class StringBuilder() {
  var content = ""

  def append(value: string): unit = {
    content = content + value
  }

  def appendLine(value: string): unit = {
    content = content + value + "\n"
  }

  override def toString(): string = content
}

case class TranspilerContext(sb: StringBuilder, symbol: Symbol)

case class Transpiler(syntaxTrees: Array[SyntaxTree], root: Symbol, outputPath: string) {
  var inUsing = false
  var inName = false

  val checker = new Checker(root)

  def transpile(): unit = {
    for (x <- 0 to (syntaxTrees.length - 1)) {
      val tree = syntaxTrees(x)
      val sourceFile = tree.file

      val name = Path.nameWithoutExtension(sourceFile.fileName) + ".pn"
      val filePath = Path.combine(outputPath, name)
      print("transpiling " + filePath + "...")

      if (!sourceFile.isScala()) {
        // skip transpiling as we are already in the target language
        File.writeAllText(filePath, sourceFile.content)
      } else {
        // transpile
        val context = new TranspilerContext(new StringBuilder(), root)
        transpileRoot(tree.root, context)
        File.writeAllText(filePath, context.sb.toString())
      }
      println("done")
    }
  }

  def transpileRoot(root: CompilationUnitSyntax, context: TranspilerContext): unit = {
    transpileNamespace(root.namespaceDeclaration, context)
    transpileUsingDirectives(root.usings, context)
    transpileMembers(root.members, context)
  }

  def transpileUsingDirectives(usings: Array[UsingDirectiveSyntax], context: TranspilerContext): unit =
    for (x <- 0 to (usings.length - 1)) {
      val usingDirective = usings(x)
      transpileUsingDirective(usingDirective, context)
    }

  def transpileUsingDirective(usingDirective: UsingDirectiveSyntax, context: TranspilerContext) = {
    inUsing = true
    transpileTokenWithText(usingDirective.usingKeyword, "using", context)
    transpileName(usingDirective.name, context)
    inUsing = false
  }

  def transpileToken(token: SyntaxToken, context: TranspilerContext): unit = {
    if (token.kind == SyntaxKind.AnnotationToken) ()
    else if (token.kind == SyntaxKind.OverrideKeyword)
      transpileTrivia(token.leading, context)
    else {
      transpileTrivia(token.leading, context)
      if (inName && token.kind == SyntaxKind.LessThanToken) {
        context.sb.append("[")
      } else if (inName && token.kind == SyntaxKind.GreaterThanToken) {
        context.sb.append("]")
      } else {
        context.sb.append(token.text)
      }
      transpileTrivia(token.trailing, context)
    }
  }

  def transpileTokenWithText(token: SyntaxToken, text: string, context: TranspilerContext): unit = {
    transpileTrivia(token.leading, context)
    context.sb.append(text)
    transpileTrivia(token.trailing, context)
  }

  def transpileTrivia(leading: Array[SyntaxTrivia], context: TranspilerContext) = {
    for (x <- 0 to (leading.length - 1)) {
      val trivia = leading(x)
      context.sb.append(trivia.text)
    }
  }


  def transpileMembers(members: Array[MemberSyntax], context: TranspilerContext): unit =
    for (x <- 0 to (members.length - 1)) {
      val member = members(x)
      transpileMember(member, context)
    }

  def transpileMember(member: MemberSyntax, context: TranspilerContext) = {
    if (member.kind == SyntaxKind.ObjectDeclaration) {
      transpileObjectDeclaration(member.objekt.get, context)
    } else if (member.kind == SyntaxKind.ClassDeclaration) {
      transpileClassDeclaration(member.klass.get, context)
    } else if (member.kind == SyntaxKind.FunctionDeclaration) {
      transpileFunctionDeclaration(member.function.get, context)
    } else if (member.kind == SyntaxKind.GlobalStatement) {
      transpileGlobalStatement(member.statement.get, context)
    } else {
      panic("unexpected member kind")
    }
  }

  def transpileObjectDeclaration(decl: ObjectDeclarationSyntax, context: TranspilerContext):unit = {
    transpileToken(decl.objectKeyword, context)
    transpileToken(decl.identifier, context)
    transpileTemplate(decl.template, context)
  }

  def transpileOptionalTemplate(template: Option[TemplateSyntax], context: TranspilerContext) =
    if (template.isEmpty) ()
    else transpileTemplate(template.get, context)

  def transpileTemplate(template: TemplateSyntax, context: TranspilerContext) = {
    transpileToken(template.openBrace, context)
    transpileMembers(template.members, context)
    transpileToken(template.closeBrace, context)
  }

  def transpileClassDeclaration(decl: ClassDeclarationSyntax, context: TranspilerContext) = {
    if(decl.caseKeyword.isDefined) {
      transpileTrivia(decl.caseKeyword.get.leading, context)
    } else ()
    transpileToken(decl.classKeyword, context)
    transpileToken(decl.identifier, context)
    transpileToken(decl.openParenToken, context)
    transpileParameters(decl.parameters, context)
    transpileToken(decl.closeParenToken, context)
    transpileOptionalTemplate(decl.template, context)
  }

  def transpileGlobalStatement(stmt: GlobalStatementSyntax, context: TranspilerContext) =
    transpileStatement(stmt.statement, context)

  def transpileStatement(statement: StatementSyntax, context: TranspilerContext) =
    if (statement.kind == SyntaxKind.BreakStatement) {
      transpileBreakStatement(statement.breakStatement.get, context)
    } else if (statement.kind == SyntaxKind.ContinueStatement) {
      transpileContinueStatement(statement.continueStatement.get, context)
    } else if (statement.kind == SyntaxKind.ExpressionStatement) {
      transpileExpressionStatement(statement.expressionStatement.get, context)
    } else if (statement.kind == SyntaxKind.VariableDeclarationStatement) {
      transpileVariableDeclarationStatement(statement.variableDeclarationStatement.get, context)
    } else {
      panic("unexpected statement kind")
    }

  def transpileVariableDeclarationStatement(get: VariableDeclarationStatementSyntax, context: TranspilerContext) = {
    transpileToken(get.valOrVarKeyword, context)
    transpileToken(get.identifier, context)
    transpileOptionalTypeAnnotation(get.typeAnnotation, context)
    transpileToken(get.equalToken, context)
    transpileExpression(get.expression, context)
  }

  def transpileExpressionStatement(get: ExpressionStatementSyntax, context: TranspilerContext): unit =
    transpileExpression(get.expression, context)

  def transpileOptionalExpression(expression: Option[ExpressionSyntax], context: TranspilerContext): unit =
    if (expression.isEmpty) ()
    else transpileExpression(expression.get, context)

  def transpileExpression(expression: ExpressionSyntax, context: TranspilerContext): unit =
    if (expression.kind == SyntaxKind.AssignmentExpression) {
      transpileAssignmentExpression(expression.assignmentExpression.get, context)
    } else if (expression.kind == SyntaxKind.BinaryExpression) {
      transpileBinaryExpression(expression.binaryExpression.get, context)
    } else if (expression.kind == SyntaxKind.ArrayCreationExpression) {
      transpileArrayCreationExpression(expression.arrayCreationExpression.get, context)
    } else if (expression.kind == SyntaxKind.AssignmentExpression) {
      transpileAssignmentExpression(expression.assignmentExpression.get, context)
    } else if (expression.kind == SyntaxKind.BinaryExpression){
      transpileBinaryExpression(expression.binaryExpression.get, context)
    } else if (expression.kind == SyntaxKind.BlockExpression){
      transpileBlockExpression(expression.blockExpression.get, context)
    } else if (expression.kind == SyntaxKind.CallExpression){
      transpileCallExpression(expression.callExpression.get, context)
    } else if (expression.kind == SyntaxKind.ForExpression) {
      transpileForExpression(expression.forExpression.get, context)
    } else if (expression.kind == SyntaxKind.GroupExpression) {
      transpileGroupExpression(expression.groupExpression.get, context)
    } else if (expression.kind == SyntaxKind.IdentifierName) {
      transpileIdentifierName(expression.identifierName.get, context)
    } else if (expression.kind == SyntaxKind.IfExpression) {
      transpileIfExpression(expression.ifExpression.get, context)
    } else if (expression.kind == SyntaxKind.IndexExpression) {
      transpileIndexExpression(expression.indexExpression.get, context)
    } else if (expression.kind == SyntaxKind.LiteralExpression) {
      transpileLiteralExpression(expression.literalExpression.get, context)
    } else if (expression.kind == SyntaxKind.MemberAccessExpression) {
      transpileMemberAccessExpression(expression.memberAccessExpression.get, context)
    } else if (expression.kind == SyntaxKind.NewExpression) {
      transpileNewExpression(expression.newExpression.get, context)
    } else if (expression.kind == SyntaxKind.UnaryExpression){
      transpileUnaryExpression(expression.unaryExpression.get, context)
    } else if (expression.kind == SyntaxKind.UnitExpression) {
      transpileUnitExpression(expression.unitExpression.get, context)
    } else if (expression.kind == SyntaxKind.WhileExpression) {
      transpileWhileExpression(expression.whileExpression.get, context)
    } else {
      panic("unexpected expression kind: " + string(expression.kind))
    }

  def transpileIdentifierName(name: IdentifierNameSyntax, context: TranspilerContext) = transpileToken(name.identifier, context)

  def transpileArrayCreationExpression(expr: ArrayCreationExpressionSyntax, context: TranspilerContext) = panic("todo")

  def transpileBinaryExpression(expr: BinaryExpressionSyntax, context: TranspilerContext) = {
    transpileExpression(expr.left, context)
    transpileToken(expr.operator, context)
    transpileExpression(expr.right, context)
  }

  def transpileBlockExpression(expr: BlockExpressionSyntax, context: TranspilerContext): unit = {
    transpileToken(expr.openBrace, context)
    transpileStatements(expr.statements, context)
    transpileOptionalExpression(expr.expression, context)
    transpileToken(expr.closeBrace, context)
  }

  def transpileStatements(statements: Array[StatementSyntax], context: TranspilerContext): unit =
    for (x <- 0 to (statements.length - 1)) {
      val statement = statements(x)
      transpileStatement(statement, context)
    }

  def transpileCallExpression(expr: CallExpressionSyntax, context: TranspilerContext): unit = {
//    checker.get_type_of_expression(expr.name, ???)
    transpileExpression(expr.name, context)
    transpileToken(expr.openParen, context)
    transpileExpressions(expr.arguments, context)
    transpileToken(expr.closeParen, context)
  }

  def transpileExpressions(arguments: ExpressionListSyntax, context: TranspilerContext): unit = {
    for (x <- 0 to (arguments.expressions.length - 1)) {
      val expression = arguments.expressions(x)
      transpileExpression(expression.expression, context)
      transpileOptionalToken(expression.separatorToken, context)
    }
  }


  def transpileForExpression(expr: ForExpressionSyntax, context: TranspilerContext): unit = {
    transpileToken(expr.forKeyword, context)
    transpileToken(expr.openParen, context)
    transpileToken(expr.identifier, context)
    transpileToken(expr.arrow, context)
    transpileExpression(expr.fromExpr, context)
    transpileToken(expr.toKeyword, context)
    transpileExpression(expr.toExpr, context)
    transpileToken(expr.closeParen, context)
    transpileExpression(expr.body, context)
  }

  def transpileGroupExpression(expr: GroupExpressionSyntax, context: TranspilerContext) = {
    transpileToken(expr.openParen, context)
    transpileExpression(expr.expression, context)
    transpileToken(expr.closeParen, context)
  }

  def transpileIfExpression(expr: IfExpressionSyntax, context: TranspilerContext): unit = {
    transpileToken(expr.ifKeyword, context)
    transpileToken(expr.openParen, context)
    transpileExpression(expr.condition, context)
    transpileToken(expr.closeParen, context)
    transpileExpression(expr.thenExpr, context)
    transpileToken(expr.elseKeyword, context)
    transpileExpression(expr.elseExpr, context)
  }

  def transpileIndexExpression(expr: IndexExpressionSyntax, context: TranspilerContext) = panic("todo")

  def transpileLiteralExpression(expr: LiteralExpressionSyntax, context: TranspilerContext) =
    transpileToken(expr.token, context)

  def transpileMemberAccessExpression(expr: MemberAccessExpressionSyntax, context: TranspilerContext):unit  = {
    transpileExpression(expr.left, context)
    transpileToken(expr.dotToken, context)
    transpileIdentifierName(expr.right, context)
  }

  def transpileNewExpression(expr: NewExpressionSyntax, context: TranspilerContext) = {
    transpileToken(expr.newKeyword, context)
    transpileName(expr.name, context)
    transpileToken(expr.openParen, context)
    transpileExpressions(expr.arguments, context)
    transpileToken(expr.closeParen, context)
  }

  def transpileUnaryExpression(expr: UnaryExpressionSyntax, context: TranspilerContext) = {
    transpileToken(expr.operator, context)
    transpileExpression(expr.expression, context)
  }

  def transpileUnitExpression(expr: UnitExpressionSyntax, context: TranspilerContext) = {
    transpileToken(expr.openParen, context)
    transpileToken(expr.closeParen, context)
  }

  def transpileWhileExpression(expr: WhileExpressionSyntax, context: TranspilerContext) = {
    transpileToken(expr.whileKeyword, context)
    transpileToken(expr.openParen, context)
    transpileExpression(expr.condition, context)
    transpileToken(expr.closeParen, context)
    transpileExpression(expr.body, context)
  }

  def transpileAssignmentExpression(expr: AssignmentExpressionSyntax, context: TranspilerContext): unit = {
    transpileExpression(expr.left, context)
    transpileToken(expr.equals, context)
    transpileExpression(expr.right, context)
  }

  def transpileContinueStatement(stmt: ContinueStatementSyntax, context: TranspilerContext) = panic("todo")

  def transpileBreakStatement(stmt: BreakStatementSyntax, context: TranspilerContext) = panic("todo")

  def transpileFunctionDeclaration(decl: FunctionDeclarationSyntax, context: TranspilerContext) = {
    transpileToken(decl.defKeyword, context)
    transpileToken(decl.identifier, context)
    transpileToken(decl.openParenToken, context)
    transpileParameters(decl.parameters, context)
    transpileToken(decl.closeParenToken, context)
    transpileOptionalTypeAnnotation(decl.typeAnnotation, context)
    transpileOptionalFunctionBody(decl.body, context)
  }

  def transpileOptionalFunctionBody(body: Option[FunctionBodySyntax], context: TranspilerContext): unit =
    if (body.isEmpty) ()
    else       transpileOptionalFunctionBody(body.get, context)


    def transpileOptionalFunctionBody(body: FunctionBodySyntax, context: TranspilerContext) = {
      transpileToken(body.equalToken, context)
      transpileExpression(body.expression, context)
    }

  def transpileTypeAnnotation(typeAnnotation: TypeAnnotationSyntax, context: TranspilerContext) = {
    transpileToken(typeAnnotation.colonToken, context)
    transpileName(typeAnnotation.typ, context)
  }

  def transpileOptionalTypeAnnotation(typeAnnotation: Option[TypeAnnotationSyntax], context: TranspilerContext) =
    if(typeAnnotation.isDefined) transpileTypeAnnotation(typeAnnotation.get, context)
    else ()

  def transpileParameters(parameters: Array[ParameterSyntax], context: TranspilerContext) =
    for (x <- 0 to (parameters.length - 1)) {
      val parameter = parameters(x)
      transpileParameter(parameter, context)
    }

  def transpileParameter(parameter: ParameterSyntax, context: TranspilerContext) = {
    transpileOptionalToken(parameter.modifier, context)
    transpileToken(parameter.identifier, context)
    transpileTypeAnnotation(parameter.typeAnnotation, context)
    transpileOptionalToken(parameter.commaToken, context)
  }

  def transpileOptionalToken(modifier: Option[SyntaxToken], context: TranspilerContext) =
    if (modifier.isEmpty) ()
    else transpileToken(modifier.get, context)

  def transpileNamespace(namespaceDeclaration: Option[NamespaceDeclarationSyntax], context: TranspilerContext): unit = {
    if (namespaceDeclaration.isEmpty) ()
    else {
      transpileToken(namespaceDeclaration.get.namespaceKeyword, context)
      transpileName(namespaceDeclaration.get.name, context)
    }
  }

  def transpileName(name: NameSyntax, context: TranspilerContext): unit = {
    if(name.kind == SyntaxKind.QualifiedName) {
      transpileQualifiedName(name.qualifiedName.get, context)
    } else {
      transpileSimpleName(name.simpleName.get, context)
    }
  }

  def transpileSimpleName(name: SimpleNameSyntax, context: TranspilerContext) =
    {
      if(name.kind == SyntaxKind.IdentifierName) {
        transpileIdentifierName(name.identifierName.get, context)
      } else if (name.kind == SyntaxKind.GenericName) {
        transpileGenericName(name.genericName.get, context)
      } else {
        panic("unexpected simple name kind")
      }
    }

  def transpileGenericName(get: GenericNameSyntax, context: TranspilerContext) = {
    transpileToken(get.identifier, context)
    transpileTypeArgumentList(get.typeArgumentlist, context)
  }

  def transpileTypeArgumentList(typeArgumentlist: TypeArgumentListSyntax, context: TranspilerContext) = {
    inName = true
    transpileTokenWithText(typeArgumentlist.lessThanToken, "<", context)
    for (x <- 0 to (typeArgumentlist.arguments.length - 1)) {
      val argument = typeArgumentlist.arguments(x)
      transpileName(argument.name, context)
      transpileOptionalToken(argument.separator, context)
    }
    transpileTokenWithText(typeArgumentlist.greaterThanToken,">", context)
    inName = false
  }


  def transpileQualifiedName(name: QualifiedNameSyntax, context: TranspilerContext) = {
    transpileName(name.left, context)
    if(inUsing && name.right.kind == SyntaxKind.IdentifierName && name.right.identifierName.get.identifier.text == "_") {
      // skip . and _ in `import bleh._`
      val ident = name.right.identifierName.get.identifier
      transpileTokenWithText(name.dotToken, "", context)
      transpileTokenWithText(ident, "", context)
    } else {
      transpileToken(name.dotToken, context)
      transpileSimpleName(name.right, context)
    }
  }
}
