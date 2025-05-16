import panther._
import MemberSyntax._
import Expression._
import StatementSyntax._
import SimpleNameSyntax._
import NameSyntax._

import scala.annotation.tailrec

case class TokenList() {
  var _items: Array[SyntaxToken] = new Array[SyntaxToken](0)
  var _size = 0

  def ensureCapacity(count: int): unit = {
    if (_size + count >= _items.length) {
      var newItems = new Array[SyntaxToken]((_size + count) * 2)
      for (i <- 0 to (_size - 1)) {
        newItems(i) = _items(i)
      }
      _items = newItems
    } else {
      ()
    }
  }

  def add(token: SyntaxToken): unit = {
    ensureCapacity(1)
    _items(_size) = token
    _size = _size + 1
  }

  def tokens(): Array[SyntaxToken] = {
    var newItems = new Array[SyntaxToken](_size)
    for (i <- 0 to (_size - 1)) {
      newItems(i) = _items(i)
    }
    newItems
  }
}

object MakeTokenList {
  def create(scanner: Lexer): Array[SyntaxToken] = {
    val tokenList = new TokenList()
    _buildTokenList(scanner, tokenList)
  }

  @tailrec
  def _buildTokenList(
      scanner: Lexer,
      tokenList: TokenList
  ): Array[SyntaxToken] = {
    val token = scanner.scan()
    tokenList.add(token)
    if (token.kind == SyntaxKind.EndOfInputToken) {
      tokenList.tokens()
    } else {
      _buildTokenList(scanner, tokenList)
    }
  }
}

case class Parser(sourceFile: SourceFile, diagnostics: DiagnosticBag) {

  val printer = new AstPrinter(true, true)

  val _tokens: Array[SyntaxToken] = {
    val scanner = new Lexer(sourceFile, diagnostics)
    MakeTokenList.create(scanner)
  }
  var _position = 0
  val scala = sourceFile.isScala()

  def next(): unit = {
    _position = _position + 1

    // skip tokens that are not valid in panther
    val curr = current()
    val kind = curr.kind
    if (
      kind == SyntaxKind.AnnotationToken || kind == SyntaxKind.OverrideKeyword
    ) {
      if (!scala) {
        diagnostics.reportUnexpectedToken(curr.location, curr.kind, kind)
      } else ()

      if (!curr.isStatementTerminator()) prependTrivia()
      else ()

      next()
    } else ()
  }

  def prependTrivia(): unit = {
    // this is a little hacky but basically we are taking any leading trivia from tokens
    // that we are throwing away and prepending it to the next token
    val len = _tokens.length
    val currP = if (_position >= len) len - 1 else _position
    val nextP = if (_position + 1 >= len) len - 1 else _position + 1
    if (currP == nextP) ()
    else {
      val next = _tokens(nextP)
      val curr = _tokens(currP)

      val newLeading =
        new Array[SyntaxTrivia](curr.leading.length + next.leading.length)
      for (i <- 0 to (curr.leading.length - 1)) {
        newLeading(i) = curr.leading(i)
      }
      for (i <- 0 to (next.leading.length - 1)) {
        newLeading(curr.leading.length + i) = next.leading(i)
      }

      _tokens(nextP) = new SyntaxToken(
        next.sourceFile,
        next.kind,
        next.start,
        next.text,
        next.value,
        newLeading,
        next.trailing
      )
    }
  }

  def debugPrint(note: string): unit = {
    val curr = current()
    //         println(note + ": " + SyntaxFacts.getKindName(curr.kind))
    //         print("  ")
    //         AstPrinter.printToken(curr)
  }

  def currentKind(): int = current().kind

  def current(): SyntaxToken = {
    val len = _tokens.length

    if (_position >= len) _tokens(len - 1)
    else _tokens(_position)
  }

  def currentPrecedence(): int = {
    val kind = currentKind()
    if (kind == SyntaxKind.EqualsToken) 1
    else if (kind == SyntaxKind.PipeToken || kind == SyntaxKind.PipePipeToken) 2
    else if (kind == SyntaxKind.CaretToken) 3
    else if (
      kind == SyntaxKind.AmpersandToken || kind == SyntaxKind.AmpersandAmpersandToken
    ) 4
    else if (
      kind == SyntaxKind.EqualsEqualsToken || kind == SyntaxKind.BangEqualsToken
    ) 5
    else if (
      kind == SyntaxKind.LessThanToken || kind == SyntaxKind.LessThanEqualsToken
    ) 6
    else if (
      kind == SyntaxKind.GreaterThanToken || kind == SyntaxKind.GreaterThanEqualsToken
    ) 6
    else if (kind == SyntaxKind.PlusToken || kind == SyntaxKind.DashToken) 7
    else if (
      kind == SyntaxKind.StarToken || kind == SyntaxKind.SlashToken || kind == SyntaxKind.PercentToken
    ) 8
    // unary/prefix is 9
    else if (
      kind == SyntaxKind.OpenParenToken || kind == SyntaxKind.OpenBracketToken
    ) 10
    else if (kind == SyntaxKind.DotToken) 10
    else if (kind == SyntaxKind.MatchKeyword) 11
    else OperatorPrecedence.Lowest
  }

  def accept(): SyntaxToken = {
    val token = current()
    next()
    // debug_print("accepted: " + SyntaxFacts.get_kind_name(token.kind))
    token
  }

  def acceptKind(kind: int): SyntaxToken = {
    val curr = current()
    if (curr.kind == kind) {
      accept()
    } else {
      diagnostics.reportUnexpectedToken(curr.location, curr.kind, kind)

      recoverKind(kind, _position, CompilerSettings.kindRecoveryAttempts)
    }
  }

  def recoverKind(kind: int, mark: int, attempts: int): SyntaxToken = {
    // enter recovery mode
    // lets search for the desired token kind assuming we already reported an error
    if (attempts == 0) {
      // we could not find the desired token kind so lets reset our position
      _position = mark

      val curr = current()

      new SyntaxToken(
        sourceFile,
        kind,
        curr.start,
        "",
        SyntaxTokenValue.Error,
        new Array[SyntaxTrivia](0),
        new Array[SyntaxTrivia](0)
      )
    } else {
      next() // skip the current token as it is not a match
      if (currentKind() == kind) {
        // we found the token lets proceed
        accept()
      } else {
        // try again
        recoverKind(kind, mark, attempts - 1)
      }
    }
  }

  def isTerminatingLine(inGroup: bool, expression: Expression): bool = {
    val isTerminating = !inGroup && hasStatementTerminator(expression)
    // debug_print("isTerminating: " + string(isTerminating))
    isTerminating
  }

  def hasStatementTerminator(expression: Expression): bool = {
    expression match {
      case value: ArrayCreationExpression =>
        value.initializer match {
          case Option.None =>
            value.closeBracket.isStatementTerminator()
          case Option.Some(value) =>
            value.closeBrace.isStatementTerminator()
        }
      case value: AssignmentExpression => hasStatementTerminator(value.right)
      case value: BinaryExpression     => hasStatementTerminator(value.right)
      case value: BlockExpression => value.closeBrace.isStatementTerminator()
      case value: CallExpression  => value.closeParen.isStatementTerminator()
      case value: ForExpression   => hasStatementTerminator(value.body)
      case value: GroupExpression => value.closeParen.isStatementTerminator()
      case Expression.IdentifierName(value) =>
        simpleNameHasStatementTerminator(value)
      case value: If =>
        hasStatementTerminator(value.elseExpr match {
          case Option.Some(value) => value.expression
          case Option.None        => value.thenExpr
        })
      case value: LiteralExpression => value.token.isStatementTerminator()
      case MatchExpression(_, _, _, _, closeBrace) =>
        closeBrace.isStatementTerminator()
      case value: MemberAccessExpression =>
        simpleNameHasStatementTerminator(value.right)
      case value: NewExpression   => value.closeParen.isStatementTerminator()
      case value: UnaryExpression => hasStatementTerminator(value.expression)
      case value: UnitExpression  => value.closeParen.isStatementTerminator()
      case value: WhileExpression => hasStatementTerminator(value.body)
    }
  }

  def simpleNameHasStatementTerminator(name: SimpleNameSyntax): bool = {
    name match {
      case SimpleNameSyntax.GenericNameSyntax(identifier, typeArgumentlist) =>
        typeArgumentlist.greaterThanToken.isStatementTerminator()
      case SimpleNameSyntax.IdentifierNameSyntax(identifier) =>
        identifier.isStatementTerminator()
      case SimpleNameSyntax.ScalaAliasSyntax(open, name, arrow, alias, close) =>
        close.isStatementTerminator()
      case SimpleNameSyntax.AliasSyntax(name, asKeyword, alias) =>
        alias.isStatementTerminator()
    }
  }

  def parseIdentifierNameExpression(): Expression = {
    val name = parseSimpleName(false)

    new IdentifierName(name)
  }

  def parseQualifiedName(inUsing: bool, left: NameSyntax): NameSyntax = {
    debugPrint("parseQualifiedName")
    if (currentKind() != SyntaxKind.DotToken) {
      left
    } else {
      val dot = accept()
      val right = parseSimpleName(inUsing)
      val qn = new QualifiedName(left, dot, right)
      parseQualifiedName(inUsing, qn)
    }
  }

  def parseName(inUsing: bool): NameSyntax = {
    debugPrint("parseName")
    val simpleName = parseSimpleName(inUsing)
    val name = new NameSyntax.SimpleName(simpleName)

    parseQualifiedName(inUsing, name)
  }

  def parseSimpleName(inUsing: bool): SimpleNameSyntax = {
    debugPrint("parseSimpleName")
    if (currentKind() == SyntaxKind.IdentifierToken) {
      val ident = accept()

      if (currentKind() == SyntaxKind.OpenBracketToken) {
        val typeArgumentlist = parseTypeArgumentList(inUsing, true)

        new GenericNameSyntax(ident, typeArgumentlist)
      } else {
        new IdentifierNameSyntax(ident)
      }
    } else if (scala && inUsing && currentKind() == SyntaxKind.StarToken) {
      val ident = accept()
      new IdentifierNameSyntax(ident)
    } else if (scala && inUsing && currentKind() == SyntaxKind.OpenBraceToken) {
      val openBrace = accept()
      val name = acceptKind(SyntaxKind.IdentifierToken)
      val arrow = acceptKind(SyntaxKind.EqualsGreaterThanToken)
      val alias = acceptKind(SyntaxKind.IdentifierToken)
      val closeBrace = acceptKind(SyntaxKind.CloseBraceToken)

      new ScalaAliasSyntax(openBrace, name, arrow, alias, closeBrace)
    } else {
      new IdentifierNameSyntax(acceptKind(SyntaxKind.IdentifierToken))
    }
  }

  def parseNamespaceDeclaration(): Option[NamespaceDeclarationSyntax] = {
    debugPrint("parseNamespaceDeclaration")
    if (currentKind() == SyntaxKind.NamespaceKeyword) {
      val keyword = accept()
      val name = parseName(false)

      Some(
        new NamespaceDeclarationSyntax(keyword, name)
      )
    } else {
      None
    }
  }

  def parseUsings(): Array[UsingDirectiveSyntax] = {
    debugPrint("parseUsings")
    var size = 0
    // TODO: support resizing
    val usings = new Array[UsingDirectiveSyntax](10)

    while (
      currentKind() == SyntaxKind.UsingKeyword || (scala && currentKind() == SyntaxKind.ImportKeyword)
    ) {
      val keyword = accept()
      val name = parseName(true)

      usings(size) = new UsingDirectiveSyntax(keyword, name)
      size = size + 1
    }

    var result = new Array[UsingDirectiveSyntax](size)
    for (i <- 0 to (size - 1)) {
      result(i) = usings(i)
    }
    result
  }

  def parseTemplate(): TemplateSyntax = {
    debugPrint("parseTemplate")
    val open = acceptKind(SyntaxKind.OpenBraceToken)
    val members = parseMembers(false)
    val close = acceptKind(SyntaxKind.CloseBraceToken)

    new TemplateSyntax(open, members, close)
  }

  def parseObjectDeclaration(): MemberSyntax = {
    debugPrint("parseObjectDeclaration")
    val objectKeyword = acceptKind(SyntaxKind.ObjectKeyword)
    val identifier = acceptKind(SyntaxKind.IdentifierToken)
    val template = parseTemplate()

    new ObjectDeclarationSyntax(objectKeyword, identifier, template)
  }

  def parseGenericTypeParameters(): Option[GenericParametersSyntax] = {
    debugPrint("parseGenericTypeParameters")

    val start = currentKind() == SyntaxKind.OpenBracketToken

    if (start) {
      val close =
        if (currentKind() == SyntaxKind.LessThanToken)
          SyntaxKind.GreaterThanToken
        else SyntaxKind.CloseBracketToken

      val lessThan = accept()

      val parameters = parseGenericTypeParameterArray()
      val greaterThan = acceptKind(close)

      Some(new GenericParametersSyntax(lessThan, parameters, greaterThan))
    } else {
      None
    }
  }

  def parseGenericTypeParameterArray()
      : SeparatedSyntaxList[GenericParameterSyntax] = {
    debugPrint("parseGenericTypeParameterList")

    var parameters = List.Cons(parseGenericParameter(), List.Nil)
    var commas: List[SyntaxToken] = List.Nil

    while (currentKind() == SyntaxKind.CommaToken) {
      commas = List.Cons(accept(), commas)
      parameters = List.Cons(parseGenericParameter(), parameters)
    }

    new SeparatedSyntaxList(parameters.reverse(), commas.reverse())
  }

  def parseGenericParameter(): GenericParameterSyntax = {
    debugPrint("parseGenericParameter")

    val hasVariance =
      (scala && (currentKind() == SyntaxKind.PlusToken || currentKind() == SyntaxKind.DashToken)) ||
        currentKind() == SyntaxKind.InKeyword ||
        currentKind() == SyntaxKind.OutKeyword

    val variance = if (hasVariance) {
      Some(accept())
    } else {
      None
    }
    val identifier = acceptKind(SyntaxKind.IdentifierToken)
    // TODO: bounds support
    new GenericParameterSyntax(variance, identifier, None)
  }

  def parseClassDeclaration(): MemberSyntax = {
    debugPrint("parseClassDeclaration")

    val caseKeyword = if (currentKind() == SyntaxKind.CaseKeyword) {
      Some(accept())
    } else {
      None
    }
    val keyword = acceptKind(SyntaxKind.ClassKeyword)
    val identifier = acceptKind(SyntaxKind.IdentifierToken)
    val typeParams = parseGenericTypeParameters()
    val open = acceptKind(SyntaxKind.OpenParenToken)
    val parameters = parseParameterList()
    val close = acceptKind(SyntaxKind.CloseParenToken)
    val template = if (currentKind() == SyntaxKind.OpenBraceToken) {
      Some(parseTemplate())
    } else {
      None
    }

    new ClassDeclarationSyntax(
      caseKeyword,
      keyword,
      identifier,
      typeParams,
      open,
      parameters,
      close,
      template
    )
  }

  def parseParameter(): ParameterSyntax = {
    debugPrint("parseParameter")
    val modifier = if (scala && currentKind() == SyntaxKind.ValKeyword) {
      Some(accept())
    } else if (scala && currentKind() == SyntaxKind.VarKeyword) {
      Some(accept())
    } else {
      None
    }
    val identifier = accept()
    val typeAnnotation = parseTypeAnnotation()
    val comma = if (currentKind() == SyntaxKind.CommaToken) {
      Some(accept())
    } else {
      None
    }
    new ParameterSyntax(modifier, identifier, typeAnnotation, comma)
  }

  def parseTypeArgumentListNames(
      inUsing: bool
  ): Array[TypeArgumentItemSyntax] = {
    debugPrint("parseTypeArgumentListNames")
    // TODO: support resizing
    val arguments = new Array[TypeArgumentItemSyntax](5)

    _parseTypeArgumentListNames(inUsing, arguments, 1)
  }

  def _parseTypeArgumentListNames(
      inUsing: bool,
      arguments: Array[TypeArgumentItemSyntax],
      size: int
  ): Array[TypeArgumentItemSyntax] = {
    val arg = parseName(inUsing)
    if (currentKind() != SyntaxKind.CommaToken) {
      arguments(size - 1) = new TypeArgumentItemSyntax(arg, None)

      // TODO: support resizing of arrays or use a different data structure

      // make a new array that is exactly the right size
      val result = new Array[TypeArgumentItemSyntax](size)
      for (i <- 0 to (size - 1)) {
        result(i) = arguments(i)
      }
      result
    } else {
      arguments(size - 1) = new TypeArgumentItemSyntax(arg, Some(accept()))
      _parseTypeArgumentListNames(inUsing, arguments, size + 1)
    }
  }

  def parseTypeArgumentList(
      inUsing: bool,
      scala: bool
  ): TypeArgumentListSyntax = {
    debugPrint("parseTypeArgumentList")
    val lessThan = accept()
    val names = parseTypeArgumentListNames(inUsing)
    val greaterThan = acceptKind(
      if (scala) SyntaxKind.CloseBracketToken else SyntaxKind.GreaterThanToken
    )

    new TypeArgumentListSyntax(lessThan, names, greaterThan)
  }

  def parseTypeAnnotation(): TypeAnnotationSyntax = {
    debugPrint("parseTypeAnnotation")
    val colon = accept()
    val typ = parseName(false)

    new TypeAnnotationSyntax(colon, typ)
  }

  def parseOptionalTypeAnnotation(): Option[TypeAnnotationSyntax] = {
    debugPrint("parseOptionalTypeAnnotation")
    if (currentKind() == SyntaxKind.ColonToken) {
      Some(parseTypeAnnotation())
    } else {
      None
    }
  }

  def parseParameterList(): List[ParameterSyntax] = {
    debugPrint("parseParameterList")
    if (currentKind() == SyntaxKind.CloseParenToken) {
      List.Nil
    } else {
      ListModule.reverse(_parseParameterList(List.Nil))
    }
  }

  def _parseParameterList(acc: List[ParameterSyntax]): List[ParameterSyntax] = {
    val param = parseParameter()
    val list = List.Cons(param, acc)

    if (param.commaToken.isEmpty()) {
      list
    } else {
      _parseParameterList(list)
    }
  }

  def parseBlockStatements(): List[StatementSyntax] = {
    debugPrint("parseBlockStatements")

    var statements: List[StatementSyntax] = List.Nil
    while (
      currentKind() != SyntaxKind.EndOfInputToken && currentKind() != SyntaxKind.CloseBraceToken && currentKind() != SyntaxKind.CaseKeyword
    ) {
      statements = List.Cons(parseStatement(), statements)
    }

    ListModule.reverse(statements)
  }

  def dropStatement(
      statements: List[StatementSyntax]
  ): List[StatementSyntax] =
    statements.take(statements.length - 1)

  def parseBlockExpressionList(): BlockExpressionListSyntax = {
    debugPrint("parseBlockExpressionList")
    val statements = parseBlockStatements()

    NonEmptyListModule.fromList(statements) match {
      case Option.None =>
        new BlockExpressionListSyntax(statements, None)
      case Option.Some(nel) =>
        val lastStatement = nel.last()
        lastStatement match {
          case StatementSyntax.ExpressionStatement(value) =>
            new BlockExpressionListSyntax(
              dropStatement(statements),
              Some(value)
            )

          case _ =>
            new BlockExpressionListSyntax(statements, None)
        }

    }
  }

  def parseBlockExpression(): Expression = {
    debugPrint("parseBlockExpression")
    val openBrace = accept()
    val exprList = parseBlockExpressionList()
    val closeBrace = acceptKind(SyntaxKind.CloseBraceToken)

    new Expression.BlockExpression(openBrace, exprList, closeBrace)
  }

  def parseGroupOrUnitExpression(): Expression = {
    debugPrint("parseGroupOrUnitExpression")
    val open = accept()
    if (currentKind() == SyntaxKind.CloseParenToken) {
      val close = accept()
      new Expression.UnitExpression(open, close)
    } else {
      val expr = parseExpression(OperatorPrecedence.Lowest)
      val close2 = acceptKind(SyntaxKind.CloseParenToken)
      new Expression.GroupExpression(open, expr, close2)
    }
  }

  def parseLiteralExpression(): Expression = {
    debugPrint("parseLiteralExpression")
    val token = accept()
    val tokenValue =
      if (token.kind == SyntaxKind.NumberToken)
        SyntaxTokenValue.Number(int(token.text))
      else if (token.kind == SyntaxKind.CharToken)
        SyntaxTokenValue.Character(token.text(0))
      else SyntaxTokenValue.String(token.text)
    new Expression.LiteralExpression(token, tokenValue)
  }

  def parseIfExpression(): Expression = {
    debugPrint("parseIfExpression")
    val keyword = accept()
    val open = acceptKind(SyntaxKind.OpenParenToken)
    val condition = parseExpression(OperatorPrecedence.Lowest)
    val close = acceptKind(SyntaxKind.CloseParenToken)
    val thenExpr = parseExpression(OperatorPrecedence.Lowest)
    val elseExpr = parseElseExpression()

    new Expression.If(
      keyword,
      open,
      condition,
      close,
      thenExpr,
      elseExpr
    )
  }

  def parseElseExpression(): Option[ElseSyntax] = {
    if (currentKind() == SyntaxKind.ElseKeyword) {
      val elseKeyword = accept()
      val expr = parseExpression(OperatorPrecedence.Lowest)
      Some(new ElseSyntax(elseKeyword, expr))
    } else {
      None
    }
  }

  def parseArrayInitializers(): ArrayInitializerExpressionSyntax = {
    debugPrint("parseArrayInitializers")
    val openBrace = accept()
    val expressions = parseExpressionList(SyntaxKind.CloseBraceToken)
    val closeBrace = acceptKind(SyntaxKind.CloseBraceToken)

    new ArrayInitializerExpressionSyntax(openBrace, expressions, closeBrace)
  }

  def parseNewExpression(): Expression = {
    debugPrint("parseNewExpression")
    val keyword = accept()
    val typ = parseName(false)

    if (currentKind() == SyntaxKind.OpenBracketToken) {
      debugPrint("parsing array creation")
      val openBracket = accept()
      val rank = if (currentKind() == SyntaxKind.CloseBracketToken) {
        None
      } else {
        Some(parseExpression(OperatorPrecedence.Lowest))
      }
      val closeBracket = acceptKind(SyntaxKind.CloseBracketToken)

      val initializer = if (currentKind() == SyntaxKind.OpenBraceToken) {
        Some(parseArrayInitializers())
      } else {
        None
      }

      new Expression.ArrayCreationExpression(
        keyword,
        typ,
        openBracket,
        rank,
        closeBracket,
        initializer
      )
    } else {
      debugPrint("parsing new expression")
      val open = acceptKind(SyntaxKind.OpenParenToken)
      val arguments = parseExpressionList(SyntaxKind.CloseParenToken)
      val close = acceptKind(SyntaxKind.CloseParenToken)

      new Expression.NewExpression(keyword, typ, open, arguments, close)
    }
  }

  def parseForExpression(): Expression = {
    val keyword = accept()
    val openParen = acceptKind(SyntaxKind.OpenParenToken)
    val variable = acceptKind(SyntaxKind.IdentifierToken)
    val arrow = acceptKind(SyntaxKind.LessThanDashToken)
    val fromExpr = parseExpression(OperatorPrecedence.Lowest)
    val toKeyword = acceptKind(SyntaxKind.ToKeyword)
    val toExpr = parseExpression(OperatorPrecedence.Lowest)
    val closeParen = acceptKind(SyntaxKind.CloseParenToken)
    val expr = parseExpression(OperatorPrecedence.Lowest)

    new Expression.ForExpression(
      keyword,
      openParen,
      variable,
      arrow,
      fromExpr,
      toKeyword,
      toExpr,
      closeParen,
      expr
    )
  }

  def parseWhileExpression(): Expression = {
    val keyword = accept()
    val openParenToken = acceptKind(SyntaxKind.OpenParenToken)
    val condition = parseExpression(OperatorPrecedence.Lowest)
    val closeParenToken = acceptKind(SyntaxKind.CloseParenToken)
    val body = parseExpression(OperatorPrecedence.Lowest)

    new Expression.WhileExpression(
      keyword,
      openParenToken,
      condition,
      closeParenToken,
      body
    )
  }

  def parseBooleanLiteralExpression(): Expression = {
    val value =
      SyntaxTokenValue.Boolean(currentKind() == SyntaxKind.TrueKeyword)

    new Expression.LiteralExpression(accept(), value)
  }

  def parseUnaryExpression(): Expression = {
    val unaryOp = accept()
    val expr = parseExpression(OperatorPrecedence.Lowest)

    new Expression.UnaryExpression(unaryOp, expr)
  }

  def parsePrefixExpression(): Expression = {
    debugPrint("parsePrefixExpression")
    val kind = currentKind()
    if (
      kind == SyntaxKind.BangToken || kind == SyntaxKind.DashToken || kind == SyntaxKind.PlusToken || kind == SyntaxKind.TildeToken
    ) {
      parseUnaryExpression()
    } else if (kind == SyntaxKind.OpenBraceToken) {
      parseBlockExpression()
    } else if (kind == SyntaxKind.IdentifierToken) {
      parseIdentifierNameExpression()
    } else if (kind == SyntaxKind.OpenParenToken) {
      parseGroupOrUnitExpression()
    } else if (
      kind == SyntaxKind.NumberToken || kind == SyntaxKind.CharToken || kind == SyntaxKind.StringToken
    ) {
      parseLiteralExpression()
    } else if (
      kind == SyntaxKind.TrueKeyword || kind == SyntaxKind.FalseKeyword
    ) {
      parseBooleanLiteralExpression()
    } else if (kind == SyntaxKind.IfKeyword) {
      parseIfExpression()
    } else if (kind == SyntaxKind.NewKeyword) {
      parseNewExpression()
    } else if (kind == SyntaxKind.ForKeyword) {
      parseForExpression()
    } else if (kind == SyntaxKind.WhileKeyword) {
      parseWhileExpression()
    } else {
      // once we stabilize make this a diagnostic
      diagnostics.reportExpectedExpression(current().location, kind)
      parseLiteralExpression()
      //            todo("parse_prefix_expression")
      //
      //            new ExpressionSyntax(
      //                0,
      //                None,
      //                None,
      //                None,
      //                None,
      //                None,
      //                Non,e
      //                None,
      //                None,
      //                None,
      //                None,
      //                None,
      //                None,
      //                None,
      //                None,
      //                None,
      //                None
      //            )
    }
  }

  def parseExpression(precedence: int): Expression =
    parseExpressionWithGroup(precedence, false)

  def parseExpressionWithGroup(precedence: int, inGroup: bool): Expression = {
    debugPrint("parseExpression")
    val left = parsePrefixExpression()

    parseInfixExpression(left, inGroup, precedence)
  }

  def parseExpressionList(terminator: int): ExpressionListSyntax = {
    debugPrint("parseExpressionList")
    val arguments: List[ExpressionItemSyntax] = List.Nil

    if (currentKind() == terminator) {
      new ExpressionListSyntax(arguments)
    } else {
      parseExpressionListInner(terminator, arguments)
    }
  }

  def parseExpressionListInner(
      terminator: int,
      arguments: List[ExpressionItemSyntax]
  ): ExpressionListSyntax = {
    val expr = parseExpressionWithGroup(OperatorPrecedence.Lowest, true)
    if (
      currentKind() == terminator || currentKind() == SyntaxKind.EndOfInputToken || currentKind() != SyntaxKind.CommaToken
    ) {
      val exprItem = new ExpressionItemSyntax(expr, None)
      new ExpressionListSyntax(
        ListModule.reverse(List.Cons(exprItem, arguments))
      )
    } else {
      val exprItem = new ExpressionItemSyntax(expr, Some(accept()))
      parseExpressionListInner(terminator, List.Cons(exprItem, arguments))
    }
  }

  def parseCallExpression(name: Expression): Expression = {
    debugPrint("parseCallExpression")
    val open = accept()
    val arguments = parseExpressionList(SyntaxKind.CloseParenToken)
    val close = acceptKind(SyntaxKind.CloseParenToken)

    new Expression.CallExpression(name, Option.None, open, arguments, close)
  }

  def parseBinaryExpression(left: Expression): Expression = {
    debugPrint("parseBinaryExpression")
    val precedence = currentPrecedence()
    val operator = accept()
    val right = parseExpression(precedence)

    new Expression.BinaryExpression(left, operator, right)
  }

  def parseMemberAccessExpression(left: Expression): Expression = {
    val dot = accept()
    val right = parseSimpleName(false)

    new Expression.MemberAccessExpression(left, dot, right)
  }

  def parseAssignmentExpression(left: Expression): Expression = {
    val equals = accept()
    val right = parseExpression(OperatorPrecedence.Lowest)

    new Expression.AssignmentExpression(left, equals, right)
  }

  def parseInfixExpression(
      left: Expression,
      inGroup: bool,
      precedence: int
  ): Expression = {
    debugPrint("parseInfixExpression")
    val kind = currentKind()
    if (isTerminatingLine(inGroup, left)) {
      left
    } else if (precedence >= currentPrecedence()) {
      // println("terminating expression due to operator precedence. kind: " + SyntaxFacts.getKindName(kind) + ", current: " + string(current_precedence()) + ", precedence: " + string(precedence))
      left
    } else {
      val expr =
        if (kind == SyntaxKind.OpenParenToken) {
          parseCallExpression(left)
        } else if (SyntaxFacts.isBinaryOperator(kind)) {
          parseBinaryExpression(left)
        } else if (kind == SyntaxKind.DotToken) {
          parseMemberAccessExpression(left)
        } else if (kind == SyntaxKind.EqualsToken) {
          parseAssignmentExpression(left)
        } else if (kind == SyntaxKind.MatchKeyword) {
          parseMatchExpression(left)
        } else {
          todo("infix")
          left
        }
      parseInfixExpression(expr, inGroup, precedence)
    }
  }

  def parseMatchExpression(left: Expression): Expression = {
    val keyword = accept()
    val open = acceptKind(SyntaxKind.OpenBraceToken)
    val cases = parseMatchCases()
    val close = acceptKind(SyntaxKind.CloseBraceToken)

    new MatchExpression(left, keyword, open, cases, close)
  }

  def parseMatchCases(): Array[MatchCaseSyntax] = {
    debugPrint("parseMatchCases")
    var size = 0

    val cases = new Array[MatchCaseSyntax](80)
    while (currentKind() == SyntaxKind.CaseKeyword) {
      cases(size) = parseMatchCase()
      size = size + 1
    }

    val result = new Array[MatchCaseSyntax](size)
    for (i <- 0 to (size - 1)) {
      result(i) = cases(i)
    }
    result
  }

  def parseMatchCase(): MatchCaseSyntax = {
    debugPrint("parseMatchCase")
    val caseKeyword = accept()
    val pattern = parsePattern()
    val arrow = acceptKind(SyntaxKind.EqualsGreaterThanToken)
    val expr = parseBlockExpressionList()

    new MatchCaseSyntax(caseKeyword, pattern, arrow, expr)
  }

  def parsePattern(): PatternSyntax = {
    debugPrint("parsePattern")

    /** Patterns are a bit tricky to parse because they can be either a type
      * pattern or an extract pattern. The difference is that a type pattern is
      * just a type name, while an extract pattern is a type name followed by a
      * list of patterns in parens.
      *
      * There are a few different forms of patterns: case x: int => ... case
      * Type.Any => ... case Type.Some(x) => ... case Any => ... case Some(x) =>
      * ... case "taco" =>
      */
    // Patterns have a few main forms:
    // 1. Type pattern: an identifier with an optional type annotation
    //
    // 2. Extract pattern: a type name with a list of patterns in parens
    if (
      currentKind() == SyntaxKind.NumberToken || currentKind() == SyntaxKind.StringToken
    ) {
      val token = accept()
      PatternSyntax.LiteralPattern(token)
    } else {
      val name = parseName(false)
      if (currentKind() == SyntaxKind.ColonToken) {
        // must be type pattern
        val typeAnnotation = parseTypeAnnotation()
        identFromName(name) match {
          case Option.Some(identifier) =>
            PatternSyntax.IdentifierPattern(identifier, typeAnnotation)
          case Option.None =>
            diagnostics.reportUnexpectedToken(
              current().location,
              currentKind(),
              SyntaxKind.IdentifierToken
            )
            PatternSyntax.TypePattern(typeAnnotation.typ)
        }

      } else if (currentKind() == SyntaxKind.OpenParenToken) {
        // must be extract pattern
        parseExtractPattern(name)
      } else {
        PatternSyntax.TypePattern(name)
      }
    }
  }

  def identFromName(name: NameSyntax): Option[SyntaxToken] = {
    name match {
      case _: QualifiedName => None
      case NameSyntax.SimpleName(simpleName) =>
        simpleName match {
          case value: SimpleNameSyntax.GenericNameSyntax =>
            None
          case SimpleNameSyntax.ScalaAliasSyntax(_, identifier, _, _, _) =>
            Some(identifier)
          case SimpleNameSyntax.AliasSyntax(identifier, _, _) =>
            Some(identifier)
          case SimpleNameSyntax.IdentifierNameSyntax(identifier) =>
            Some(identifier)
        }
    }
  }

  def parseExtractPattern(nameSyntax: NameSyntax): PatternSyntax = {
    val open = accept()
    val patterns = parsePatternList()
    val close = acceptKind(SyntaxKind.CloseParenToken)
    PatternSyntax.ExtractPattern(nameSyntax, open, patterns, close)
  }

  def parsePatternList(): Array[PatternItemSyntax] = {
    debugPrint("parsePatternList")
    var size = 0
    val patterns = new Array[PatternItemSyntax](20)

    _parsePatternList(patterns, 1)
  }

  def _parsePatternList(
      array: Array[PatternItemSyntax],
      i: int
  ): Array[PatternItemSyntax] = {
    val pattern = parsePattern()

    if (currentKind() == SyntaxKind.CommaToken) {
      array(i - 1) = PatternItemSyntax(pattern, Some(accept()))
      _parsePatternList(array, i + 1)
    } else {
      array(i - 1) = PatternItemSyntax(pattern, None)
      val result = new Array[PatternItemSyntax](i)
      for (j <- 0 to (i - 1)) {
        result(j) = array(j)
      }
      result
    }
  }

  def todo(note: string): unit = {
    val curr = current()
    printer.printTokenInfo(curr)
    panic(note + ": " + SyntaxFacts.getKindName(curr.kind))
  }

  def parseFunctionBody(): Option[FunctionBodySyntax] = {
    debugPrint("parseFunctionBody")
    if (currentKind() == SyntaxKind.EqualsToken) {
      val equal = accept()
      val expr = parseExpression(OperatorPrecedence.Lowest)
      Some(new FunctionBodySyntax(equal, expr))
    } else {
      None
    }
  }

  def parseFunctionDeclaration(): MemberSyntax = {
    debugPrint("parseFunctionDeclaration")
    val defKeyword = accept()
    val identifier = acceptKind(SyntaxKind.IdentifierToken)
    val typeParams = parseGenericTypeParameters()
    val openParenToken = acceptKind(SyntaxKind.OpenParenToken)
    val parameters = parseParameterList()
    val closeParenToken = acceptKind(SyntaxKind.CloseParenToken)
    val typeAnnotation = parseOptionalTypeAnnotation()
    val body = parseFunctionBody()

    new FunctionDeclarationSyntax(
      defKeyword,
      identifier,
      typeParams,
      openParenToken,
      parameters,
      closeParenToken,
      typeAnnotation,
      body
    )
  }

  def parseEnumDeclaration(): MemberSyntax = {
    debugPrint("parseEnumDeclaration")

    val enumKeyword = accept()
    val identifier = acceptKind(SyntaxKind.IdentifierToken)
    val typeParams = parseGenericTypeParameters()
    val open = acceptKind(SyntaxKind.OpenBraceToken)
    val cases = parseEnumCases()
    val members = parseMembers(false)
    val close = acceptKind(SyntaxKind.CloseBraceToken)

    new EnumDeclarationSyntax(
      enumKeyword,
      identifier,
      typeParams,
      open,
      cases,
      members,
      close
    )
  }

  def parseEnumCases(): Array[EnumCaseSyntax] = {
    debugPrint("parseEnumCases")
    var size = 0
    // TODO: support resizing
    val cases = new Array[EnumCaseSyntax](100)

    while (
      currentKind() != SyntaxKind.EndOfInputToken && currentKind() == SyntaxKind.CaseKeyword
    ) {
      cases(size) = parseEnumCase()
      size = size + 1
    }

    val result = new Array[EnumCaseSyntax](size)
    for (i <- 0 to (size - 1)) {
      result(i) = cases(i)
    }
    result
  }

  def parseEnumCase(): EnumCaseSyntax = {
    debugPrint("parseEnumCase")
    val keyword = accept()
    val identifier = acceptKind(SyntaxKind.IdentifierToken)
    val enumParams = if (currentKind() == SyntaxKind.OpenParenToken) {
      Some(parseEnumCaseParameters())
    } else {
      None
    }

    new EnumCaseSyntax(keyword, identifier, enumParams)
  }

  def parseEnumCaseParameters(): EnumCaseParametersSyntax = {
    debugPrint("parseEnumCaseParameters")
    val open = accept()
    val parameters = parseParameterList()
    val close = acceptKind(SyntaxKind.CloseParenToken)

    new EnumCaseParametersSyntax(open, parameters, close)
  }

  def parseBreakStatement(): StatementSyntax = {
    debugPrint("parseBreakStatement")

    val keyword = accept()

    new StatementSyntax.BreakStatement(keyword)
  }

  def parseContinueStatement(): StatementSyntax = {
    debugPrint("parseContinueStatement")

    val keyword = accept()

    new StatementSyntax.ContinueStatement(keyword)
  }

  def parseExpressionStatement(): StatementSyntax = {
    debugPrint("parseExpressionStatement")
    val expr = parseExpression(OperatorPrecedence.Lowest)
    // assert_statement_terminator(expr)
    new StatementSyntax.ExpressionStatement(expr)
  }

  def parseVariableDeclarationStatement(): StatementSyntax = {
    debugPrint("parseVariableDeclarationStatement")
    val keyword = accept()
    val ident = acceptKind(SyntaxKind.IdentifierToken)
    val typeAnnotation = parseOptionalTypeAnnotation()
    val eq = acceptKind(SyntaxKind.EqualsToken)
    val expr = parseExpression(OperatorPrecedence.Lowest)

    new StatementSyntax.VariableDeclarationStatement(
      keyword,
      ident,
      typeAnnotation,
      eq,
      expr
    )
  }

  def parseStatement(): StatementSyntax = {
    debugPrint("parseStatement")
    val kind = currentKind()
    if (kind == SyntaxKind.ValKeyword || kind == SyntaxKind.VarKeyword) {
      parseVariableDeclarationStatement()
    } else if (kind == SyntaxKind.BreakKeyword) {
      parseBreakStatement()
    } else if (kind == SyntaxKind.ContinueKeyword) {
      parseContinueStatement()
    } else {
      parseExpressionStatement()
    }
  }

  def parseGlobalStatement(): MemberSyntax.GlobalStatementSyntax = {
    debugPrint("parseGlobalStatement")
    new GlobalStatementSyntax(parseStatement())
  }

  def parseMember(topLevelStatement: bool): MemberSyntax = {
    debugPrint("parseMember")
    val kind = currentKind()
    if (kind == SyntaxKind.ObjectKeyword) {
      parseObjectDeclaration()
    } else if (
      kind == SyntaxKind.CaseKeyword && scala ||
      kind == SyntaxKind.ClassKeyword
    ) {
      parseClassDeclaration()
    } else if (kind == SyntaxKind.DefKeyword) {
      parseFunctionDeclaration()
    } else if (kind == SyntaxKind.EnumKeyword) {
      parseEnumDeclaration()
    } else if (kind == SyntaxKind.ValKeyword || kind == SyntaxKind.VarKeyword) {
      parseVariableDeclaration()
    } else {
      parseGlobalStatement()
    }
  }

  def parseVariableDeclaration(): MemberSyntax = {
    val keyword = accept()
    val ident = acceptKind(SyntaxKind.IdentifierToken)
    val typeAnnotation = parseOptionalTypeAnnotation()
    val eq = acceptKind(SyntaxKind.EqualsToken)
    val expr = parseExpression(OperatorPrecedence.Lowest)

    new MemberSyntax.VariableDeclaration(
      keyword,
      ident,
      typeAnnotation,
      eq,
      expr
    )
  }

  def parseMembers(topLevelStatement: bool): List[MemberSyntax] = {
    debugPrint("parseMembers")
    var members: List[MemberSyntax] = List.Nil

    while (
      currentKind() != SyntaxKind.EndOfInputToken && currentKind() != SyntaxKind.CloseBraceToken
    ) {
      members = List.Cons(parseMember(topLevelStatement), members)
    }

    ListModule.reverse(members)
  }

  def parseCompilationUnit(): CompilationUnitSyntax = {
    debugPrint("parseCompilationUnit")
    val namespaceDeclaration = parseNamespaceDeclaration()
    val usingDirectives = parseUsings()
    val members = parseMembers(true)
    val endToken = acceptKind(SyntaxKind.EndOfInputToken)
    new CompilationUnitSyntax(
      namespaceDeclaration,
      usingDirectives,
      members,
      endToken
    )
  }
}

case class SyntaxTree(
    file: SourceFile,
    root: CompilationUnitSyntax,
    diagnostics: Diagnostics
)
