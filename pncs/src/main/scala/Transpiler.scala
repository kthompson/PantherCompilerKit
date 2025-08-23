import Expression._
import panther._
import system.io._
import MemberSyntax._
import PatternSyntax._
import NameSyntax._
import SimpleNameSyntax._

case class TranspilerContext(sb: StringBuilder)

case class Transpiler(
    syntaxTrees: List[SyntaxTree],
    outputPath: string
) {
  var inUsing = false
  var inGenericTypeArgumentList = false

  //  val checker = new Checker(root)

  def transpile(): unit = {
    _transpile(syntaxTrees)
  }

  def _transpile(trees: List[SyntaxTree]): unit = {
    trees match {
      case List.Nil => ()
      case List.Cons(tree, tail) =>
        transpileTree(tree)
        _transpile(tail)
    }
  }

  def transpileTree(tree: SyntaxTree): unit = {
    val sourceFile = tree.file

    val name = Path.nameWithoutExtension(sourceFile.fileName) + ".pn"
    val filePath = Path.combine(outputPath, name)
    print("transpiling " + filePath + "...")

    if (!sourceFile.isScala()) {
      // skip transpiling as we are already in the target language
      File.writeAllText(filePath, sourceFile.content)
    } else {
      // transpile
      val context = new TranspilerContext(new StringBuilder())
      transpileRoot(tree.root, context)
      File.writeAllText(filePath, context.sb.toString())
    }
    println("done")

  }

  def transpileRoot(
      root: CompilationUnitSyntax,
      context: TranspilerContext
  ): unit = {
    transpileNamespace(root.namespaceDeclaration, context)
    transpileUsingDirectives(root.usings, context)
    transpileMembers(root.members, context)
  }

  def transpileUsingDirectives(
      usings: Array[UsingDirectiveSyntax],
      context: TranspilerContext
  ): unit =
    for (x <- 0 to (usings.length - 1)) {
      val usingDirective = usings(x)
      transpileUsingDirective(usingDirective, context)
    }

  def transpileUsingDirective(
      usingDirective: UsingDirectiveSyntax,
      context: TranspilerContext
  ): unit = {
    inUsing = true
    transpileTokenWithText(usingDirective.usingKeyword, "using", context)
    transpileName(usingDirective.name, context)
    inUsing = false
  }

  def transpileToken(token: SyntaxToken, context: TranspilerContext): unit = {
    if (token.kind == SyntaxKind.AnnotationToken) ()
    else if (token.kind == SyntaxKind.OverrideKeyword)
      transpileTrivia(token.leading, context)
    else if (
      token.kind == SyntaxKind.IdentifierToken && token.text == "String"
    ) {
      transpileTokenWithText(token, "string", context)
    } else if (
      token.kind == SyntaxKind.IdentifierToken && token.text == "Int"
    ) {
      transpileTokenWithText(token, "int", context)
    } else if (
      token.kind == SyntaxKind.IdentifierToken && token.text == "Unit"
    ) {
      transpileTokenWithText(token, "unit", context)
    } else if (
      token.kind == SyntaxKind.IdentifierToken && token.text == "Boolean"
    ) {
      transpileTokenWithText(token, "bool", context)
    } else {
      transpileTrivia(token.leading, context)
      if (inGenericTypeArgumentList && token.kind == SyntaxKind.LessThanToken) {
        context.sb.append("[")
      } else if (
        inGenericTypeArgumentList && token.kind == SyntaxKind.GreaterThanToken
      ) {
        context.sb.append("]")
      } else {
        context.sb.append(token.text)
      }
      transpileTrivia(token.trailing, context)
    }
  }

  def transpileTokenWithText(
      token: SyntaxToken,
      text: string,
      context: TranspilerContext
  ): unit = {
    transpileTrivia(token.leading, context)
    context.sb.append(text)
    transpileTrivia(token.trailing, context)
  }

  def transpileTrivia(
      leading: Array[SyntaxTrivia],
      context: TranspilerContext
  ): unit = {
    for (x <- 0 to (leading.length - 1)) {
      val trivia = leading(x)
      context.sb.append(trivia.text)
    }
  }

  def transpileMembers(
      members: List[MemberSyntax],
      context: TranspilerContext
  ): unit = {
    members match {
      case List.Nil => ()
      case List.Cons(head, tail) =>
        transpileMember(head, context)
        transpileMembers(tail, context)
    }
  }

  def transpileMember(
      member: MemberSyntax,
      context: TranspilerContext
  ): unit = {
    member match {
      case value: ObjectDeclarationSyntax =>
        transpileObjectDeclaration(value, context)
      case value: ClassDeclarationSyntax =>
        transpileClassDeclaration(value, context)
      case value: FunctionDeclarationSyntax =>
        transpileFunctionDeclaration(value, context)
      case value: EnumDeclarationSyntax =>
        transpileEnumDeclaration(value, context)
      case value: VariableDeclaration =>
        transpileVariableDeclaration(value, context)
      case value: GlobalStatementSyntax =>
        transpileGlobalStatement(value, context)
    }
  }

  def transpileEnumDeclaration(
      decl: MemberSyntax.EnumDeclarationSyntax,
      context: TranspilerContext
  ): unit = {
    transpileToken(decl.enumKeyword, context)
    transpileToken(decl.identifier, context)
    transpileGenericParameters(decl.genericParameters, context)
    transpileToken(decl.openBraceToken, context)
    transpileEnumCases(decl.cases, context)
    transpileMembers(decl.members, context)
    transpileToken(decl.closeBraceToken, context)
  }

  def transpileGenericParameters(
      typeParameters: Option[GenericParametersSyntax],
      context: TranspilerContext
  ): unit = {
    typeParameters match {
      case Option.Some(value) =>
        transpileToken(value.lessThanToken, context)
        transpileGenericParameterArray(
          value.parameters.items,
          value.parameters.separators,
          context
        )
        transpileToken(value.greaterThanToken, context)
      case Option.None =>
    }
  }

  def transpileGenericParameterArray(
      items: List[GenericParameterSyntax],
      separators: List[SyntaxToken],
      context: TranspilerContext
  ): unit = {
    items match {
      case List.Nil => ()
      case List.Cons(parameter, remainingParameters) =>
        transpileGenericParameter(parameter, context)
        separators match {
          case List.Nil => ()
          case List.Cons(comma, remainingCommas) =>
            transpileToken(comma, context)
            transpileGenericParameterArray(
              remainingParameters,
              remainingCommas,
              context
            )
        }
    }
  }

  def transpileGenericParameter(
      parameter: GenericParameterSyntax,
      context: TranspilerContext
  ): unit = {
    parameter.variance match {
      case Option.Some(value) =>
        val text = if (value.kind == SyntaxKind.PlusToken) "in " else "out "
        transpileTokenWithText(value, text, context)
      case _ =>
    }
    transpileToken(parameter.identifier, context)
  }

  def transpileEnumCases(
      cases: Array[EnumCaseSyntax],
      context: TranspilerContext
  ): unit = {
    for (x <- 0 to (cases.length - 1)) {
      val enumCase = cases(x)
      transpileEnumCase(enumCase, context)
    }
  }

  def transpileEnumCase(
      enumCase: EnumCaseSyntax,
      context: TranspilerContext
  ): unit = {
    transpileToken(enumCase.caseKeyword, context)
    transpileToken(enumCase.identifier, context)
    enumCase.parameters match {
      case Option.Some(value) => transpileEnumCaseParameters(value, context)
      case Option.None        => ()
    }
  }

  def transpileEnumCaseParameters(
      parameters: EnumCaseParametersSyntax,
      context: TranspilerContext
  ): unit = {
    transpileToken(parameters.openParenToken, context)
    transpileParameters(parameters.parameters, context)
    transpileToken(parameters.closeParenToken, context)
  }

  def transpileObjectDeclaration(
      decl: MemberSyntax.ObjectDeclarationSyntax,
      context: TranspilerContext
  ): unit = {
    transpileToken(decl.objectKeyword, context)
    transpileToken(decl.identifier, context)
    transpileTemplate(decl.template, context)
  }

  def transpileOptionalTemplate(
      template: Option[TemplateSyntax],
      context: TranspilerContext
  ): unit =
    template match {
      case Option.None =>
      case Option.Some(value) =>
        transpileTemplate(value, context)
    }

  def transpileTemplate(
      template: TemplateSyntax,
      context: TranspilerContext
  ): unit = {
    transpileToken(template.openBrace, context)
    transpileMembers(template.members, context)
    transpileToken(template.closeBrace, context)
  }

  def transpileClassDeclaration(
      decl: MemberSyntax.ClassDeclarationSyntax,
      context: TranspilerContext
  ): unit = {
    decl.caseKeyword match {
      case Option.Some(value) => transpileTrivia(value.leading, context)
      case _                  =>
    }
    transpileToken(decl.classKeyword, context)
    transpileToken(decl.identifier, context)
    transpileGenericParameters(decl.genericParameters, context)
    transpileToken(decl.openParenToken, context)
    transpileParameters(decl.parameters, context)
    transpileToken(decl.closeParenToken, context)
    transpileOptionalTemplate(decl.template, context)
  }

  def transpileGlobalStatement(
      stmt: MemberSyntax.GlobalStatementSyntax,
      context: TranspilerContext
  ): unit =
    transpileStatement(stmt.statement, context)

  def transpileStatement(
      statement: StatementSyntax,
      context: TranspilerContext
  ): unit = {
    statement match {
      case value: StatementSyntax.VariableDeclarationStatement =>
        transpileVariableDeclarationStatement(value, context)
      case value: StatementSyntax.BreakStatement =>
        transpileBreakStatement(value, context)
      case value: StatementSyntax.ContinueStatement =>
        transpileContinueStatement(value, context)
      case value: StatementSyntax.ExpressionStatement =>
        transpileExpressionStatement(value, context)
    }
  }

  def transpileVariableDeclaration(
      get: MemberSyntax.VariableDeclaration,
      context: TranspilerContext
  ): unit = {
    transpileToken(get.valOrVarKeyword, context)
    transpileToken(get.identifier, context)
    transpileOptionalTypeAnnotation(get.typeAnnotation, context)
    transpileToken(get.equalToken, context)
    transpileExpression(get.expression, context)
  }

  def transpileVariableDeclarationStatement(
      get: StatementSyntax.VariableDeclarationStatement,
      context: TranspilerContext
  ): unit = {
    transpileToken(get.valOrVarKeyword, context)
    transpileToken(get.identifier, context)
    transpileOptionalTypeAnnotation(get.typeAnnotation, context)
    transpileToken(get.equalToken, context)
    transpileExpression(get.expression, context)
  }

  def transpileExpressionStatement(
      get: StatementSyntax.ExpressionStatement,
      context: TranspilerContext
  ): unit =
    transpileExpression(get.expression, context)

  def transpileOptionalExpression(
      expression: Option[Expression],
      context: TranspilerContext
  ): unit =
    expression match {
      case Option.None        =>
      case Option.Some(value) => transpileExpression(value, context)
    }

  def transpileExpression(
      expression: Expression,
      context: TranspilerContext
  ): unit = {
    expression match {
      case value: Expression.ArrayCreationExpression =>
        transpileArrayCreationExpression(value, context)
      case value: Expression.AssignmentExpression =>
        transpileAssignmentExpression(value, context)
      case value: Expression.BinaryExpression =>
        transpileBinaryExpression(value, context)
      case value: Expression.BlockExpression =>
        transpileBlockExpression(value, context)
      case value: Expression.CallExpression =>
        transpileCallExpression(value, context)
      case value: Expression.CastExpression =>
        transpileCastExpression(value, context)
      case value: Expression.ForExpression =>
        transpileForExpression(value, context)
      case value: Expression.GroupExpression =>
        transpileGroupExpression(value, context)
      case Expression.IdentifierName(value) =>
        transpileSimpleName(value, context)
      case value: Expression.If => transpileIfExpression(value, context)
      case value: Expression.LiteralExpression =>
        transpileLiteralExpression(value, context)
      case value: Expression.MatchExpression =>
        transpileMatchExpression(value, context)
      case value: Expression.MemberAccessExpression =>
        transpileMemberAccessExpression(value, context)
      case value: Expression.NewExpression =>
        transpileNewExpression(value, context)
      case value: Expression.UnaryExpression =>
        transpileUnaryExpression(value, context)
      case value: Expression.UnitExpression =>
        transpileUnitExpression(value, context)
      case value: Expression.WhileExpression =>
        transpileWhileExpression(value, context)
    }
  }

  def transpileMatchExpression(
      expr: Expression.MatchExpression,
      context: TranspilerContext
  ): unit = {
    transpileExpression(expr.expression, context)
    transpileToken(expr.matchKeyword, context)
    transpileToken(expr.openBrace, context)
    transpileMatchCases(expr.cases, context)
    transpileToken(expr.closeBrace, context)
  }

  def transpileMatchCases(
      cases: Array[MatchCaseSyntax],
      context: TranspilerContext
  ): unit = {
    for (x <- 0 to (cases.length - 1)) {
      val case_ = cases(x)
      transpileMatchCase(case_, context)
    }
  }

  def transpileMatchCase(
      matchCase: MatchCaseSyntax,
      context: TranspilerContext
  ): unit = {
    transpileToken(matchCase.caseKeyword, context)
    transpilePattern(matchCase.pattern, context)
    transpileToken(matchCase.arrow, context)
    transpileBlockExpressionList(matchCase.block, context)
  }

  def transpilePattern(
      pattern: PatternSyntax,
      context: TranspilerContext
  ): unit = {
    pattern match {
      case Identifier(identifier, typeAnnotation) =>
        transpileToken(identifier, context)
        transpileTypeAnnotation(typeAnnotation, context)
      case Type(value) =>
        transpileName(value, context)
      case Literal(value) => transpileToken(value, context)
      case Discard(value) => transpileToken(value, context)
      case Extract(name, open, patterns, close) =>
        transpileName(name, context)
        transpileToken(open, context)
        transpilePatterns(patterns, context)
        transpileToken(close, context)
    }
  }

  def transpilePatterns(
      patterns: Array[PatternItemSyntax],
      context: TranspilerContext
  ): unit = {
    for (x <- 0 to (patterns.length - 1)) {
      val pattern = patterns(x)
      transpilePattern(pattern.pattern, context)
      pattern.separatorToken match {
        case Option.Some(value) => transpileToken(value, context)
        case Option.None        => ()
      }
    }
  }

  def transpileIdentifierName(
      name: SimpleNameSyntax.IdentifierNameSyntax,
      context: TranspilerContext
  ): unit =
    transpileToken(name.identifier, context)

  def transpileArrayCreationExpression(
      expr: Expression.ArrayCreationExpression,
      context: TranspilerContext
  ): unit = panic("todo")

  def transpileBinaryExpression(
      expr: Expression.BinaryExpression,
      context: TranspilerContext
  ): unit = {
    transpileExpression(expr.left, context)
    transpileToken(expr.operator, context)
    transpileExpression(expr.right, context)
  }

  def transpileBlockExpression(
      expr: Expression.BlockExpression,
      context: TranspilerContext
  ): unit = {
    transpileToken(expr.openBrace, context)
    transpileBlockExpressionList(expr.block, context)
    transpileToken(expr.closeBrace, context)
  }

  def transpileBlockExpressionList(
      node: BlockExpressionListSyntax,
      context: TranspilerContext
  ): unit = {
    transpileStatements(node.statements, context)
    transpileOptionalExpression(node.expression, context)
  }

  def transpileStatements(
      statements: List[StatementSyntax],
      context: TranspilerContext
  ): unit = {
    statements match {
      case List.Nil => ()
      case List.Cons(head, tail) =>
        transpileStatement(head, context)
        transpileStatements(tail, context)
    }
  }

  def transpileCallExpression(
      expr: Expression.CallExpression,
      context: TranspilerContext
  ): unit = {
    //    checker.get_type_of_expression(expr.name, ???)
    transpileExpression(expr.name, context)
    transpileToken(expr.openParen, context)
    transpileExpressionList(expr.arguments, context)
    transpileToken(expr.closeParen, context)
  }

  def transpileCastExpression(
      expr: Expression.CastExpression,
      context: TranspilerContext
  ): unit = {
    transpileExpression(expr.expression, context)
    transpileToken(expr.asKeyword, context)
    transpileName(expr.typ, context)
  }

  def transpileExpressionItems(
      arguments: List[ExpressionItemSyntax],
      context: TranspilerContext
  ): unit = {
    arguments match {
      case List.Nil => ()
      case List.Cons(head, tail) => {
        transpileExpression(head.expression, context)
        transpileOptionalToken(head.separatorToken, context)
        transpileExpressionItems(tail, context)
      }
    }
  }

  def transpileExpressionList(
      arguments: ExpressionListSyntax,
      context: TranspilerContext
  ): unit = {
    transpileExpressionItems(arguments.expressions, context)
  }

  def transpileForExpression(
      expr: Expression.ForExpression,
      context: TranspilerContext
  ): unit = {
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

  def transpileGroupExpression(
      expr: Expression.GroupExpression,
      context: TranspilerContext
  ): unit = {
    transpileToken(expr.openParen, context)
    transpileExpression(expr.expression, context)
    transpileToken(expr.closeParen, context)
  }

  def transpileIfExpression(
      expr: Expression.If,
      context: TranspilerContext
  ): unit = {
    transpileToken(expr.ifKeyword, context)
    transpileToken(expr.openParen, context)
    transpileExpression(expr.condition, context)
    transpileToken(expr.closeParen, context)
    transpileExpression(expr.thenExpr, context)
    expr.elseExpr match {
      case Option.None        => ()
      case Option.Some(value) => transpileElseExpression(value, context)
    }
  }

  def transpileElseExpression(
      expr: ElseSyntax,
      context: TranspilerContext
  ): unit = {
    transpileToken(expr.elseKeyword, context)
    transpileExpression(expr.expression, context)
  }

  def transpileLiteralExpression(
      expr: Expression.LiteralExpression,
      context: TranspilerContext
  ): unit =
    transpileToken(expr.token, context)

  def transpileMemberAccessExpression(
      expr: Expression.MemberAccessExpression,
      context: TranspilerContext
  ): unit = {
    transpileExpression(expr.left, context)
    transpileToken(expr.dotToken, context)
    transpileSimpleName(expr.right, context)
  }

  def transpileNewExpression(
      expr: Expression.NewExpression,
      context: TranspilerContext
  ): unit = {
    transpileToken(expr.newKeyword, context)
    transpileName(expr.name, context)
    transpileToken(expr.openParen, context)
    transpileExpressionList(expr.arguments, context)
    transpileToken(expr.closeParen, context)
  }

  def transpileUnaryExpression(
      expr: Expression.UnaryExpression,
      context: TranspilerContext
  ): unit = {
    transpileToken(expr.operator, context)
    transpileExpression(expr.expression, context)
  }

  def transpileUnitExpression(
      expr: Expression.UnitExpression,
      context: TranspilerContext
  ): unit = {
    transpileToken(expr.openParen, context)
    transpileToken(expr.closeParen, context)
  }

  def transpileWhileExpression(
      expr: Expression.WhileExpression,
      context: TranspilerContext
  ): unit = {
    transpileToken(expr.whileKeyword, context)
    transpileToken(expr.openParen, context)
    transpileExpression(expr.condition, context)
    transpileToken(expr.closeParen, context)
    transpileExpression(expr.body, context)
  }

  def transpileAssignmentExpression(
      expr: Expression.AssignmentExpression,
      context: TranspilerContext
  ): unit = {
    transpileExpression(expr.left, context)
    transpileToken(expr.equals, context)
    transpileExpression(expr.right, context)
  }

  def transpileContinueStatement(
      stmt: StatementSyntax.ContinueStatement,
      context: TranspilerContext
  ): unit = panic("todo")

  def transpileBreakStatement(
      stmt: StatementSyntax.BreakStatement,
      context: TranspilerContext
  ): unit = panic("todo")

  def transpileFunctionDeclaration(
      decl: MemberSyntax.FunctionDeclarationSyntax,
      context: TranspilerContext
  ): unit = {
    transpileToken(decl.defKeyword, context)
    transpileToken(decl.identifier, context)
    transpileGenericParameters(decl.genericParameters, context)
    transpileToken(decl.openParenToken, context)
    transpileParameters(decl.parameters, context)
    transpileToken(decl.closeParenToken, context)
    transpileOptionalTypeAnnotation(decl.typeAnnotation, context)
    transpileOptionalFunctionBody(decl.body, context)
  }

  def transpileOptionalFunctionBody(
      body: Option[FunctionBodySyntax],
      context: TranspilerContext
  ): unit =
    body match {
      case Option.None        => ()
      case Option.Some(value) => transpileFunctionBody(value, context)
    }

  def transpileFunctionBody(
      body: FunctionBodySyntax,
      context: TranspilerContext
  ): unit = {
    transpileToken(body.equalToken, context)
    transpileExpression(body.expression, context)
  }

  def transpileTypeAnnotation(
      typeAnnotation: TypeAnnotationSyntax,
      context: TranspilerContext
  ): unit = {
    transpileToken(typeAnnotation.colonToken, context)
    transpileName(typeAnnotation.typ, context)
  }

  def transpileOptionalTypeAnnotation(
      typeAnnotation: Option[TypeAnnotationSyntax],
      context: TranspilerContext
  ): unit =
    typeAnnotation match {
      case Option.Some(value) =>
        transpileTypeAnnotation(value, context)
      case Option.None =>
    }

  def transpileParameters(
      parameters: List[ParameterSyntax],
      context: TranspilerContext
  ): unit = {
    parameters match {
      case List.Nil =>
      case List.Cons(head, tail) =>
        transpileParameter(head, context)
        transpileParameters(tail, context)
    }
  }

  def transpileParameter(
      parameter: ParameterSyntax,
      context: TranspilerContext
  ): unit = {
//    the modifier is not currently supported
//    transpileOptionalToken(parameter.modifier, context)
    transpileToken(parameter.identifier, context)
    transpileTypeAnnotation(parameter.typeAnnotation, context)
    transpileOptionalToken(parameter.commaToken, context)
  }

  def transpileOptionalToken(
      modifier: Option[SyntaxToken],
      context: TranspilerContext
  ): unit =
    modifier match {
      case Option.None        => ()
      case Option.Some(value) => transpileToken(value, context)
    }

  def transpileNamespace(
      namespaceDeclaration: Option[NamespaceDeclarationSyntax],
      context: TranspilerContext
  ): unit = {
    namespaceDeclaration match {
      case Option.Some(value) =>
        transpileToken(value.namespaceKeyword, context)
        transpileName(value.name, context)
      case Option.None =>
    }
  }

  def transpileName(name: NameSyntax, context: TranspilerContext): unit = {
    name match {
      case value: NameSyntax.QualifiedName =>
        transpileQualifiedName(value, context)
      case NameSyntax.SimpleName(value) =>
        transpileSimpleName(value, context)
    }
  }

  def transpileSimpleName(
      name: SimpleNameSyntax,
      context: TranspilerContext
  ): unit = {
    name match {
      case value: SimpleNameSyntax.GenericNameSyntax =>
        transpileGenericName(value, context)
      case value: SimpleNameSyntax.IdentifierNameSyntax =>
        transpileIdentifierName(value, context)
      case value: SimpleNameSyntax.AliasSyntax =>
        ??? // todo
      case value: SimpleNameSyntax.ScalaAliasSyntax =>
        ??? // todo
    }
  }

  def transpileGenericName(
      get: SimpleNameSyntax.GenericNameSyntax,
      context: TranspilerContext
  ): unit = {
    transpileToken(get.identifier, context)
    transpileTypeArgumentList(get.typeArgumentlist, context)
  }

  def transpileTypeArgumentList(
      typeArgumentlist: TypeArgumentListSyntax,
      context: TranspilerContext
  ): unit = {
    inGenericTypeArgumentList = true
    transpileToken(typeArgumentlist.lessThanToken, context)
    for (x <- 0 to (typeArgumentlist.arguments.length - 1)) {
      val argument = typeArgumentlist.arguments(x)
      transpileName(argument.name, context)
      transpileOptionalToken(argument.separator, context)
    }
    transpileToken(typeArgumentlist.greaterThanToken, context)
    inGenericTypeArgumentList = false
  }

  def transpileQualifiedName(
      name: NameSyntax.QualifiedName,
      context: TranspilerContext
  ): unit = {
    transpileName(name.left, context)
    name.right match {
      case _: SimpleNameSyntax.GenericNameSyntax =>
        transpileToken(name.dotToken, context)
        transpileSimpleName(name.right, context)
      case SimpleNameSyntax.IdentifierNameSyntax(identifier) =>
        if (isWildcardImport(identifier)) {
          // skip . and _ in `import bleh._`
          transpileTokenWithText(name.dotToken, "", context)
          transpileTokenWithText(identifier, "", context)
        } else {
          transpileToken(name.dotToken, context)
          transpileSimpleName(name.right, context)
        }
      case alias: SimpleNameSyntax.AliasSyntax =>
        ???
      case alias: SimpleNameSyntax.ScalaAliasSyntax =>
        transpileToken(name.dotToken, context)
        transpileTokenWithText(alias.open, "", context)
        transpileToken(alias.name, context)
        transpileTokenWithText(alias.arrow, "as", context)
        transpileToken(alias.alias, context)
        transpileTokenWithText(alias.close, "", context)
    }
  }

  def isWildcardImport(identifier: SyntaxToken): bool = {
    if (!inUsing) false
    else {
      val text = identifier.text
      text == "_" || text == "*"
    }
  }
}
