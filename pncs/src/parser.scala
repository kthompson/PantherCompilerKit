import panther._
import MemberSyntax._
import ExpressionSyntax._
import StatementSyntax._
import SimpleNameSyntax._
import NameSyntax._

import scala.annotation.tailrec

case class TokenList() {
  var _items: Array[SyntaxToken] = new Array[SyntaxToken](0)
  var _size = 0

  def ensure_capacity(count: int): unit = {
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
    ensure_capacity(1)
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
    _build_token_list(scanner, tokenList)
  }

  @tailrec
  def _build_token_list(scanner: Lexer, tokenList: TokenList): Array[SyntaxToken] = {
    val token = scanner.scan()
    tokenList.add(token)
    if (token.kind == SyntaxKind.EndOfInputToken) {
      tokenList.tokens()
    } else {
      _build_token_list(scanner, tokenList)
    }
  }
}


case class Parser(source_file: SourceFile, diagnostics: DiagnosticBag) {

  val _tokens: Array[SyntaxToken] = {
    val scanner = new Lexer(source_file, diagnostics)
    MakeTokenList.create(scanner)
  }
  var _position = 0

  def next(): unit = {
    _position = _position + 1

    // skip tokens that are not valid in panther
    val curr = current()
    val kind = curr.kind
    if (kind == SyntaxKind.AnnotationToken || kind == SyntaxKind.OverrideKeyword) {
      if (!source_file.isScala()) {
        diagnostics.reportUnexpectedToken(curr.location, curr.kind, kind)
      } else ()

      if (!curr.is_statement_terminator()) prependTrivia()
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

      val newLeading = new Array[SyntaxTrivia](curr.leading.length + next.leading.length)
      for (i <- 0 to (curr.leading.length - 1)) {
        newLeading(i) = curr.leading(i)
      }
      for (i <- 0 to (next.leading.length - 1)) {
        newLeading(curr.leading.length + i) = next.leading(i)
      }

      _tokens(nextP) = new SyntaxToken(
        next.source_file,
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

  def current_kind(): int = current().kind

  def current(): SyntaxToken = {
    val len = _tokens.length

    if (_position >= len) _tokens(len - 1)
    else _tokens(_position)
  }

  def current_precedence(): int = {
    val kind = current_kind()
    if (kind == SyntaxKind.EqualsToken) 1
    else if (kind == SyntaxKind.PipeToken || kind == SyntaxKind.PipePipeToken) 2
    else if (kind == SyntaxKind.CaretToken) 3
    else if (kind == SyntaxKind.AmpersandToken || kind == SyntaxKind.AmpersandAmpersandToken) 4
    else if (kind == SyntaxKind.EqualsEqualsToken || kind == SyntaxKind.BangEqualsToken) 5
    else if (kind == SyntaxKind.LessThanToken || kind == SyntaxKind.LessThanEqualsToken) 6
    else if (kind == SyntaxKind.GreaterThanToken || kind == SyntaxKind.GreaterThanEqualsToken) 6
    else if (kind == SyntaxKind.PlusToken || kind == SyntaxKind.DashToken) 7
    else if (kind == SyntaxKind.StarToken || kind == SyntaxKind.SlashToken) 8
    // unary/prefix is 9
    else if (kind == SyntaxKind.OpenParenToken || kind == SyntaxKind.OpenBracketToken) 10
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

  def accept(kind: int): SyntaxToken = {
    val curr = current()
    if (curr.kind == kind) {
      accept()
    } else {
      diagnostics.reportUnexpectedToken(curr.location, curr.kind, kind)
      new SyntaxToken(source_file, kind, curr.start, "", MakeSyntaxTokenValue.none(), new Array[SyntaxTrivia](0), new Array[SyntaxTrivia](0))
    }
  }

  def is_terminating_line(in_group: bool, expression: ExpressionSyntax): bool = {
    val is_terminating = !in_group && hasStatementTerminator(expression)
    // debug_print("is_terminating: " + string(is_terminating))
    is_terminating
  }

  def hasStatementTerminator(expression: ExpressionSyntax): bool = {
    expression match {
      case ArrayCreationExpression(value) =>
        if (value.initializer.isDefined) {
          value.initializer.get.closeBrace.is_statement_terminator()
        } else {
          value.closeBracket.is_statement_terminator()
        }
      case AssignmentExpression(value) => hasStatementTerminator(value.right)
      case BinaryExpression(value) => hasStatementTerminator(value.right)
      case BlockExpression(value) => value.closeBrace.is_statement_terminator()
      case CallExpression(value) => value.closeParen.is_statement_terminator()
      case ForExpression(value) => hasStatementTerminator(value.body)
      case GroupExpression(value) => value.closeParen.is_statement_terminator()
      case IdentifierName(value) => value.identifier.is_statement_terminator()
      case IfExpression(value) => hasStatementTerminator(value.elseExpr)
      case IndexExpression(value) => value.closeBracket.is_statement_terminator()
      case LiteralExpression(value) => value.token.is_statement_terminator()
      case MatchExpression(_, _, _, _, closeBrace) => closeBrace.is_statement_terminator()
      case MemberAccessExpression(value) => value.right.identifier.is_statement_terminator()
      case NewExpression(value) => value.closeParen.is_statement_terminator()
      case UnaryExpression(value) => hasStatementTerminator(value.expression)
      case UnitExpression(value) => value.closeParen.is_statement_terminator()
      case WhileExpression(value) => hasStatementTerminator(value.body)
    }
  }

  def parse_identifier_name(): SimpleNameSyntax.IdentifierNameSyntax = {
    debugPrint("parse_identifier_name")
    val identifier = accept(SyntaxKind.IdentifierToken)

    new IdentifierNameSyntax(identifier)
  }

  def parse_identifier_name_expression(): ExpressionSyntax = {
    val name = parse_identifier_name()

    new IdentifierName(name)
  }

  def parseQualifiedName(left: NameSyntax): NameSyntax = {
    debugPrint("parse_qualified_name")
    if (current_kind() != SyntaxKind.DotToken) {
      left
    } else {
      val dot = accept()
      val right = parse_simple_name()
      val qn = new QualifiedNameSyntax(left, dot, right)
      parseQualifiedName(qn)
    }
  }

  def parseName(): NameSyntax = {
    debugPrint("parse_name")
    val simple_name = parse_simple_name()
    val name = new NameSyntax.SimpleName(simple_name)

    parseQualifiedName(name)
  }

  def parse_simple_name(): SimpleNameSyntax = {
    debugPrint("parse_simple_name")
    if (current_kind() == SyntaxKind.IdentifierToken) {
      val ident = accept()

      if (current_kind() == SyntaxKind.LessThanToken) {
        val typeArgumentlist = parse_type_argument_list(false)
        new GenericNameSyntax(ident, typeArgumentlist)

      } else if (source_file.isScala() && current_kind() == SyntaxKind.OpenBracketToken) {
        val typeArgumentlist = parse_type_argument_list(true)

        new GenericNameSyntax(ident, typeArgumentlist)
      } else {
        new IdentifierNameSyntax(ident)
      }
    } else if (source_file.isScala() && current_kind() == SyntaxKind.OpenBraceToken) {
      val openBrace = accept()
      val name = accept(SyntaxKind.IdentifierToken)
      val arrow = accept(SyntaxKind.EqualsGreaterThanToken)
      val alias = accept(SyntaxKind.IdentifierToken)
      val closeBrace = accept(SyntaxKind.CloseBraceToken)

      new ScalaAliasSyntax(openBrace, name, arrow, alias, closeBrace)
    } else {
      new IdentifierNameSyntax(accept(SyntaxKind.IdentifierToken))
    }
  }

  def parse_namespace_declaration(): Option[NamespaceDeclarationSyntax] = {
    debugPrint("parse_namespace_declaration")
    if (current_kind() == SyntaxKind.NamespaceKeyword) {
      val keyword = accept()
      val name = parseName()

      Some(
        new NamespaceDeclarationSyntax(keyword, name)
      )
    } else {
      None
    }
  }

  def parse_usings(): Array[UsingDirectiveSyntax] = {
    debugPrint("parse_usings")
    var size = 0
    // TODO: support resizing
    val usings = new Array[UsingDirectiveSyntax](10)
    val isScala = source_file.isScala()

    while (current_kind() == SyntaxKind.UsingKeyword || (isScala && current_kind() == SyntaxKind.ImportKeyword)) {
      val keyword = accept()
      val name = parseName()

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
    debugPrint("parse_template")
    val open = accept(SyntaxKind.OpenBraceToken)
    val members = parseMembers(false)
    val close = accept(SyntaxKind.CloseBraceToken)

    new TemplateSyntax(open, members, close)
  }

  def parse_object_declaration(): MemberSyntax = {
    debugPrint("parse_object_declaration")
    val objectKeyword = accept(SyntaxKind.ObjectKeyword)
    val identifier = accept(SyntaxKind.IdentifierToken)
    val template = parseTemplate()

    new ObjectDeclarationSyntax(objectKeyword, identifier, template)
  }

  def parse_class_declaration(): MemberSyntax = {
    debugPrint("parse_class_declaration")

    val caseKeyword = if (current_kind() == SyntaxKind.CaseKeyword) {
      Some(accept())
    } else {
      None
    }
    val keyword = accept(SyntaxKind.ClassKeyword)
    val identifier = accept(SyntaxKind.IdentifierToken)
    val open = accept(SyntaxKind.OpenParenToken)
    val parameters = parse_parameter_list()
    val close = accept(SyntaxKind.CloseParenToken)
    val template = if (current_kind() == SyntaxKind.OpenBraceToken) {
      Some(parseTemplate())
    } else {
      None
    }

    new ClassDeclarationSyntax(caseKeyword, keyword, identifier, open, parameters, close, template)
  }

  def parse_parameter(): ParameterSyntax = {
    debugPrint("parse_parameter")
    val scala = source_file.isScala()
    val modifier = if (scala && current_kind() == SyntaxKind.ValKeyword) {
      Some(accept())
    } else if (scala && current_kind() == SyntaxKind.VarKeyword) {
      Some(accept())
    } else {
      None
    }
    val identifier = accept()
    val typeAnnotation = parseTypeAnnotation()
    val comma = if (current_kind() == SyntaxKind.CommaToken) {
      Some(accept())
    } else {
      None
    }
    new ParameterSyntax(modifier, identifier, typeAnnotation, comma)
  }

  def parse_type_argument_list_names(): Array[TypeArgumentItemSyntax] = {
    debugPrint("parse_type_argument_list_names")
    // TODO: support resizing
    val arguments = new Array[TypeArgumentItemSyntax](5)

    _parse_type_argument_list_names(arguments, 1)
  }

  def _parse_type_argument_list_names(arguments: Array[TypeArgumentItemSyntax], size: int): Array[TypeArgumentItemSyntax] = {
    val arg = parseName()
    if (current_kind() != SyntaxKind.CommaToken) {
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
      _parse_type_argument_list_names(arguments, size + 1)
    }
  }

  def parse_type_argument_list(scala: bool): TypeArgumentListSyntax = {
    debugPrint("parse_type_argument_list")
    val lessThan = accept()
    val names = parse_type_argument_list_names()
    val greaterThan = accept(if (scala) SyntaxKind.CloseBracketToken else SyntaxKind.GreaterThanToken)

    new TypeArgumentListSyntax(lessThan, names, greaterThan)
  }

  def parseTypeAnnotation(): TypeAnnotationSyntax = {
    debugPrint("parse_type_annotation")
    val colon = accept()
    val typ = parseName()

    new TypeAnnotationSyntax(colon, typ)
  }

  def parseOptionalTypeAnnotation(): Option[TypeAnnotationSyntax] = {
    debugPrint("parse_optional_type_annotation")
    if (current_kind() == SyntaxKind.ColonToken) {
      Some(parseTypeAnnotation())
    } else {
      None
    }
  }

  def parse_parameter_list(): Array[ParameterSyntax] = {
    debugPrint("parse_parameter_list")
    if (current_kind() == SyntaxKind.CloseParenToken) {
      new Array[ParameterSyntax](0)
    } else {
      // TODO: support resizing
      val parameters = new Array[ParameterSyntax](20)

      _parse_parameter_list(parameters, 1)
    }
  }

  def _parse_parameter_list(parameters: Array[ParameterSyntax], size: int): Array[ParameterSyntax] = {
    val param = parse_parameter()
    parameters(size - 1) = param

    if (param.commaToken.isEmpty) {
      val result = new Array[ParameterSyntax](size)
      for (i <- 0 to (size - 1)) {
        result(i) = parameters(i)
      }
      result
    } else {
      _parse_parameter_list(parameters, size + 1)
    }
  }

  def parse_block_statements(): Array[StatementSyntax] = {
    debugPrint("parse_block_statements")
    var size = 0

    var statements = new Array[StatementSyntax](20)
    while (current_kind() != SyntaxKind.EndOfInputToken && current_kind() != SyntaxKind.CloseBraceToken && current_kind() != SyntaxKind.CaseKeyword) {
      if (size >= statements.length) {
        // resize
        val newStatements = new Array[StatementSyntax](statements.length * 2)
        for (i <- 0 to (size - 1)) {
          newStatements(i) = statements(i)
        }
        statements = newStatements
      } else ()
      statements(size) = parse_statement()
      size = size + 1
    }

    val result = new Array[StatementSyntax](size)
    for (i <- 0 to (size - 1)) {
      result(i) = statements(i)
    }
    result
  }

  def drop_statement(statements: Array[StatementSyntax]): Array[StatementSyntax] = {
    val size = statements.length - 1
    val result = new Array[StatementSyntax](size)
    for (i <- 0 to (size - 1)) {
      result(i) = statements(i)
    }
    result
  }

  def parseBlockExpressionList(): BlockExpressionListSyntax = {
    debugPrint("parseBlockExpressionList")
    val statements = parse_block_statements()

    if (statements.length > 0) {
      val lastStatement = statements(statements.length - 1)
      lastStatement match {
        case StatementSyntax.ExpressionStatement(value) =>
          new BlockExpressionListSyntax(
            drop_statement(statements),
            Some(value.expression)
          )

        case _ =>
          new BlockExpressionListSyntax(statements, None)
      }
    } else {
      new BlockExpressionListSyntax(statements, None)
    }
  }

  def parse_block_expression(): ExpressionSyntax = {
    debugPrint("parse_block_expression")
    val openBrace = accept()
    val exprList = parseBlockExpressionList()
    val closeBrace = accept(SyntaxKind.CloseBraceToken)

    new BlockExpression(new BlockExpressionSyntax(openBrace, exprList, closeBrace))
  }

  def parse_group_or_unit_expression(): ExpressionSyntax = {
    debugPrint("parse_group_or_unit_expression")
    val open = accept()
    if (current_kind() == SyntaxKind.CloseParenToken) {
      val close = accept()
      val unit_expr = new UnitExpressionSyntax(open, close)
      new UnitExpression(unit_expr)
    } else {
      val expr = parse_expression(OperatorPrecedence.Lowest)
      val close2 = accept(SyntaxKind.CloseParenToken)
      val group = new GroupExpressionSyntax(open, expr, close2)

      new GroupExpression(group)
    }
  }

  def parse_literal_expression(): ExpressionSyntax = {
    debugPrint("parse_literal_expression")
    val token = accept()
    val tokenValue = if (token.kind == SyntaxKind.NumberToken) MakeSyntaxTokenValue.number(int(token.text))
    else if (token.kind == SyntaxKind.CharToken) MakeSyntaxTokenValue.char(token.text(0))
    else MakeSyntaxTokenValue.string(token.text)
    val literal = new LiteralExpressionSyntax(token, tokenValue)

    new LiteralExpression(literal)
  }

  def parse_if_expression(): ExpressionSyntax = {
    debugPrint("parse_if_expression")
    val keyword = accept()
    val open = accept(SyntaxKind.OpenParenToken)
    val condition = parse_expression(OperatorPrecedence.Lowest)
    val close = accept(SyntaxKind.CloseParenToken)
    val thenExpr = parse_expression(OperatorPrecedence.Lowest)
    val elseKeyword = accept(SyntaxKind.ElseKeyword)
    val elseExpr = parse_expression(OperatorPrecedence.Lowest)

    val expr = new IfExpressionSyntax(keyword, open, condition, close, thenExpr, elseKeyword, elseExpr)

    new IfExpression(expr)
  }

  def parse_array_initializers(): ArrayInitializerExpressionSyntax = {
    debugPrint("parse_array_initializers")
    val openBrace = accept()
    val expressions = parse_expression_list(SyntaxKind.CloseBraceToken)
    val closeBrace = accept(SyntaxKind.CloseBraceToken)

    new ArrayInitializerExpressionSyntax(openBrace, expressions, closeBrace)
  }

  def parse_new_expression(): ExpressionSyntax = {
    debugPrint("parse_new_expression")
    val keyword = accept()
    val typ = parseName()

    if (current_kind() == SyntaxKind.OpenBracketToken) {
      debugPrint("parsing array creation")
      val openBracket = accept()
      val rank = if (current_kind() == SyntaxKind.CloseBracketToken) {
        None
      } else {
        Some(parse_expression(OperatorPrecedence.Lowest))
      }
      val closeBracket = accept(SyntaxKind.CloseBracketToken)

      val initializer = if (current_kind() == SyntaxKind.OpenBraceToken) {
        Some(parse_array_initializers())
      } else {
        None
      }

      val arrayExpr = new ArrayCreationExpressionSyntax(
        keyword,
        typ,
        openBracket,
        rank,
        closeBracket,
        initializer
      )

      new ArrayCreationExpression(arrayExpr)
    } else {
      debugPrint("parsing new expression")
      val open = accept(SyntaxKind.OpenParenToken)
      val arguments = parse_expression_list(SyntaxKind.CloseParenToken)
      val close = accept(SyntaxKind.CloseParenToken)

      val expr = new NewExpressionSyntax(keyword, typ, open, arguments, close)

      new NewExpression(expr)
    }
  }

  def parse_for_expression(): ExpressionSyntax = {
    val keyword = accept()
    val openParen = accept(SyntaxKind.OpenParenToken)
    val variable = accept(SyntaxKind.IdentifierToken)
    val arrow = accept(SyntaxKind.LessThanDashToken)
    val fromExpr = parse_expression(OperatorPrecedence.Lowest)
    val toKeyword = accept(SyntaxKind.ToKeyword)
    val toExpr = parse_expression(OperatorPrecedence.Lowest)
    val closeParen = accept(SyntaxKind.CloseParenToken)
    val expr = parse_expression(OperatorPrecedence.Lowest)

    val forExpr = new ForExpressionSyntax(
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

    new ForExpression(forExpr)
  }

  def parse_while_expression(): ExpressionSyntax = {
    val keyword = accept()
    val openParenToken = accept(SyntaxKind.OpenParenToken)
    val condition = parse_expression(OperatorPrecedence.Lowest)
    val closeParenToken = accept(SyntaxKind.CloseParenToken)
    val body = parse_expression(OperatorPrecedence.Lowest)

    val expr = new WhileExpressionSyntax(
      keyword,
      openParenToken,
      condition,
      closeParenToken,
      body
    )

    new WhileExpression(expr)
  }

  def parse_boolean_literal_expression(): ExpressionSyntax = {
    val value = MakeSyntaxTokenValue.bool(current_kind() == SyntaxKind.TrueKeyword)

    val expr = new LiteralExpressionSyntax(accept(), value)

    new LiteralExpression(expr)
  }

  def parse_unary_expression(): ExpressionSyntax = {
    val unary_op = accept()
    val expr = parse_expression(OperatorPrecedence.Lowest)

    val unaryExpr = new UnaryExpressionSyntax(unary_op, expr)

    new UnaryExpression(unaryExpr)
  }

  def parsePrefixExpression(): ExpressionSyntax = {
    debugPrint("parse_prefix_expression")
    val kind = current_kind()
    if (kind == SyntaxKind.BangToken || kind == SyntaxKind.DashToken || kind == SyntaxKind.PlusToken || kind == SyntaxKind.TildeToken) {
      parse_unary_expression()
    } else if (kind == SyntaxKind.OpenBraceToken) {
      parse_block_expression()
    } else if (kind == SyntaxKind.IdentifierToken) {
      parse_identifier_name_expression()
    } else if (kind == SyntaxKind.OpenParenToken) {
      parse_group_or_unit_expression()
    } else if (kind == SyntaxKind.NumberToken || kind == SyntaxKind.CharToken || kind == SyntaxKind.StringToken) {
      parse_literal_expression()
    } else if (kind == SyntaxKind.TrueKeyword || kind == SyntaxKind.FalseKeyword) {
      parse_boolean_literal_expression()
    } else if (kind == SyntaxKind.IfKeyword) {
      parse_if_expression()
    } else if (kind == SyntaxKind.NewKeyword) {
      parse_new_expression()
    } else if (kind == SyntaxKind.ForKeyword) {
      parse_for_expression()
    } else if (kind == SyntaxKind.WhileKeyword) {
      parse_while_expression()
    } else {
      // once we stablize make this a diagnostic
      diagnostics.reportExpectedExpression(current().location, kind)
      parse_literal_expression()
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

  def parse_expression(precedence: int): ExpressionSyntax =
    parse_expression(precedence, false)

  def parse_expression(precedence: int, in_group: bool): ExpressionSyntax = {
    debugPrint("parse_expression")
    val left = parsePrefixExpression()

    parse_infix_expression(left, in_group, precedence)
  }

  def parse_expression_list(terminator: int): ExpressionListSyntax = {
    debugPrint("parse_expression_list")
    // TODO: support resizing
    val arguments = new Array[ExpressionItemSyntax](20)

    if (current_kind() == terminator) {
      new ExpressionListSyntax(new Array[ExpressionItemSyntax](0))
    } else {
      parse_expression_list_inner(terminator, arguments, 1)
    }
  }

  def parse_expression_list_inner(terminator: int, arguments: Array[ExpressionItemSyntax], size: int): ExpressionListSyntax = {
    val expr = parse_expression(OperatorPrecedence.Lowest, false)
    if (current_kind() == terminator || current_kind() == SyntaxKind.EndOfInputToken || current_kind() != SyntaxKind.CommaToken) {
      arguments(size - 1) = new ExpressionItemSyntax(expr, None)

      val result = new Array[ExpressionItemSyntax](size)
      for (i <- 0 to (size - 1)) {
        result(i) = arguments(i)
      }
      new ExpressionListSyntax(result)
    } else {
      arguments(size - 1) = new ExpressionItemSyntax(expr, Some(accept()))
      parse_expression_list_inner(terminator, arguments, size + 1)
    }
  }

  def parse_call_expression(name: ExpressionSyntax): ExpressionSyntax = {
    debugPrint("parse_call_expression")
    val open = accept()
    val arguments = parse_expression_list(SyntaxKind.CloseParenToken)
    val close = accept(SyntaxKind.CloseParenToken)

    val callExpression = new CallExpressionSyntax(name, open, arguments, close)
    new CallExpression(callExpression)
  }

  def parse_binary_expression(left: ExpressionSyntax): ExpressionSyntax = {
    debugPrint("parse_binary_expression")
    val precedence = current_precedence()
    val operator = accept()
    val right = parse_expression(precedence)

    val expr = new BinaryExpressionSyntax(left, operator, right)

    new BinaryExpression(expr)
  }

  def parse_member_access_expression(left: ExpressionSyntax): ExpressionSyntax = {
    val dot = accept()
    val right = parse_identifier_name()

    val expr = new MemberAccessExpressionSyntax(left, dot, right)

    new MemberAccessExpression(expr)
  }

  def parse_index_expression(left: ExpressionSyntax): ExpressionSyntax = {
    val openBracket = accept()
    val index = parse_expression(OperatorPrecedence.Lowest)
    val closeBracket = accept(SyntaxKind.CloseBracketToken)

    val expr = new IndexExpressionSyntax(left, openBracket, index, closeBracket)

    new IndexExpression(expr)
  }

  def parse_assignment_expression(left: ExpressionSyntax): ExpressionSyntax = {
    val equals = accept()
    val right = parse_expression(OperatorPrecedence.Lowest)

    val expr = new AssignmentExpressionSyntax(left, equals, right)

    new AssignmentExpression(expr)
  }

  def parse_infix_expression(left: ExpressionSyntax, in_group: bool, precedence: int): ExpressionSyntax = {
    debugPrint("parse_infix_expression")
    val kind = current_kind()
    if (is_terminating_line(in_group, left)) {
      left
    } else if (precedence >= current_precedence()) {
      // println("terminating expression due to operator precedence. kind: " + SyntaxFacts.getKindName(kind) + ", current: " + string(current_precedence()) + ", precedence: " + string(precedence))
      left
    } else {
      val expr =
        if (kind == SyntaxKind.OpenParenToken) {
          parse_call_expression(left)
        } else if (SyntaxFacts.isBinaryOperator(kind)) {
          parse_binary_expression(left)
        } else if (kind == SyntaxKind.DotToken) {
          parse_member_access_expression(left)
        } else if (kind == SyntaxKind.OpenBracketToken) {
          parse_index_expression(left)
        } else if (kind == SyntaxKind.EqualsToken) {
          parse_assignment_expression(left)
        } else if (kind == SyntaxKind.MatchKeyword) {
          parse_match_expression(left)
        } else {
          todo("infix")
          left
        }
      parse_infix_expression(expr, in_group, precedence)
    }
  }

  def parse_match_expression(left: ExpressionSyntax): ExpressionSyntax = {
    val keyword = accept()
    val open = accept(SyntaxKind.OpenBraceToken)
    val cases = parse_match_cases()
    val close = accept(SyntaxKind.CloseBraceToken)

    new MatchExpression(left, keyword, open, cases, close)
  }

  def parse_match_cases(): Array[MatchCaseSyntax] = {
    debugPrint("parse_match_cases")
    var size = 0

    val cases = new Array[MatchCaseSyntax](80)
    while (current_kind() == SyntaxKind.CaseKeyword) {
      cases(size) = parse_match_case()
      size = size + 1
    }

    val result = new Array[MatchCaseSyntax](size)
    for (i <- 0 to (size - 1)) {
      result(i) = cases(i)
    }
    result
  }

  def parse_match_case(): MatchCaseSyntax = {
    debugPrint("parse_match_case")
    val caseKeyword = accept()
    val pattern = parse_pattern()
    val arrow = accept(SyntaxKind.EqualsGreaterThanToken)
    val expr = parseBlockExpressionList()

    new MatchCaseSyntax(caseKeyword, pattern, arrow, expr)
  }

  def parse_pattern(): PatternSyntax = {
    debugPrint("parse_pattern")

    /**
     * Patterns are a bit tricky to parse because they can be either a type pattern or an extract pattern.
     * The difference is that a type pattern is just a type name, while an extract pattern is a type name
     * followed by a list of patterns in parens.
     *
     * There are a few different forms of patterns:
     * case x: int => ...
     * case Type.Any => ...
     * case Type.Some(x) => ...
     * case Any => ...
     * case Some(x) => ...
     */
    // Patterns have a few main forms:
    // 1. Type pattern: an identifier with an optional type annotation
    //
    // 2. Extract pattern: a type name with a list of patterns in parens

    val name = parseName()
    if (current_kind() == SyntaxKind.ColonToken) {
      // must be type pattern
      val typeAnnotation = parseTypeAnnotation()
      identFromName(name) match {
        case Some(identifier) =>
          PatternSyntax.IdentifierPattern(identifier, typeAnnotation)
        case None =>
          diagnostics.reportUnexpectedToken(current().location, current_kind(), SyntaxKind.IdentifierToken)
          PatternSyntax.TypePattern(typeAnnotation.typ)
      }

    } else if (current_kind() == SyntaxKind.OpenParenToken) {
      // must be extract pattern
      parseExtractPattern(name)
    } else {
      PatternSyntax.TypePattern(name)
    }
  }

  def identFromName(name: NameSyntax): Option[SyntaxToken] = {
    name match {
      case _: QualifiedNameSyntax => None
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
    val close = accept(SyntaxKind.CloseParenToken)
    PatternSyntax.ExtractPattern(nameSyntax, open, patterns, close)
  }

  def parsePatternList(): Array[PatternItemSyntax] = {
    debugPrint("parsePatternList")
    var size = 0
    val patterns = new Array[PatternItemSyntax](20)

    _parsePatternList(patterns, 1)
  }

  def _parsePatternList(array: Array[PatternItemSyntax], i: int): Array[PatternItemSyntax] = {
    val pattern = parse_pattern()

    if (current_kind() == SyntaxKind.CommaToken) {
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
    AstPrinter.printTokenInfo(curr)
    panic(note + ": " + SyntaxFacts.getKindName(curr.kind))
  }

  def parse_function_body(): Option[FunctionBodySyntax] = {
    debugPrint("parse_function_body")
    if (current_kind() == SyntaxKind.EqualsToken) {
      val equal = accept()
      val expr = parse_expression(OperatorPrecedence.Lowest)
      Some(new FunctionBodySyntax(equal, expr))
    } else {
      None
    }
  }

  def parse_function_declaration(): MemberSyntax = {
    debugPrint("parse_function_declaration")
    val defKeyword = accept()
    val identifier = accept(SyntaxKind.IdentifierToken)
    val openParenToken = accept(SyntaxKind.OpenParenToken)
    val parameters = parse_parameter_list()
    val closeParenToken = accept(SyntaxKind.CloseParenToken)
    val typeAnnotation = parseOptionalTypeAnnotation()
    val body = parse_function_body()


    new FunctionDeclarationSyntax(
      defKeyword,
      identifier,
      openParenToken,
      parameters,
      closeParenToken,
      typeAnnotation,
      body
    )
  }

  def parseEnumDeclaration(): MemberSyntax = {
    debugPrint("parse_enum_declaration")

    val enumKeyword = accept()
    val identifier = accept(SyntaxKind.IdentifierToken)
    val open = accept(SyntaxKind.OpenBraceToken)
    val cases = parseEnumCases()
    val members = parseMembers(false)
    val close = accept(SyntaxKind.CloseBraceToken)

    new EnumDeclarationSyntax(enumKeyword, identifier, open, cases, members, close)
  }

  def parseEnumCases(): Array[EnumCaseSyntax] = {
    debugPrint("parseEnumCases")
    var size = 0
    // TODO: support resizing
    val cases = new Array[EnumCaseSyntax](100)

    while (current_kind() != SyntaxKind.EndOfInputToken && current_kind() != SyntaxKind.CloseBraceToken) {
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
    val identifier = accept(SyntaxKind.IdentifierToken)
    val enumParams = if (current_kind() == SyntaxKind.OpenParenToken) {
      Some(parseEnumCaseParameters())
    } else {
      None
    }

    new EnumCaseSyntax(keyword, identifier, enumParams)
  }

  def parseEnumCaseParameters(): EnumCaseParametersSyntax = {
    debugPrint("parseEnumCaseParameters")
    val open = accept()
    val parameters = parse_parameter_list()
    val close = accept(SyntaxKind.CloseParenToken)

    new EnumCaseParametersSyntax(open, parameters, close)
  }

  def parse_break_statement(): StatementSyntax = {
    debugPrint("parse_break_statement")

    val keyword = accept()

    val stmt = new BreakStatementSyntax(keyword)

    new StatementSyntax.BreakStatement(stmt)
  }

  def parse_continue_statement(): StatementSyntax = {
    debugPrint("parse_continue_statement")

    val keyword = accept()

    val stmt = new ContinueStatementSyntax(keyword)

    new StatementSyntax.ContinueStatement(stmt)
  }

  def parse_expression_statement(): StatementSyntax = {
    debugPrint("parse_expression_statement")
    val expr = parse_expression(OperatorPrecedence.Lowest)
    // assert_statement_terminator(expr)
    new ExpressionStatement(new ExpressionStatementSyntax(expr))
  }

  def parse_variable_declaration_statement(): StatementSyntax = {
    debugPrint("parse_variable_declaration_statement")
    val keyword = accept()
    val ident = accept(SyntaxKind.IdentifierToken)
    val typeAnnotation = parseOptionalTypeAnnotation()
    val eq = accept(SyntaxKind.EqualsToken)
    val expr = parse_expression(OperatorPrecedence.Lowest)

    val decl = new VariableDeclarationStatementSyntax(
      keyword,
      ident,
      typeAnnotation,
      eq,
      expr
    )

    new StatementSyntax.VariableDeclarationStatement(decl)
  }


  def parse_statement(): StatementSyntax = {
    debugPrint("parse_statement")
    val kind = current_kind()
    if (kind == SyntaxKind.ValKeyword || kind == SyntaxKind.VarKeyword) {
      parse_variable_declaration_statement()
    } else if (kind == SyntaxKind.BreakKeyword) {
      parse_break_statement()
    } else if (kind == SyntaxKind.ContinueKeyword) {
      parse_continue_statement()
    } else {
      parse_expression_statement()
    }
  }

  def parse_global_statement(): MemberSyntax.GlobalStatementSyntax = {
    debugPrint("parse_global_statement")
    new GlobalStatementSyntax(parse_statement())
  }

  def parseMember(topLevelStatement: bool): MemberSyntax = {
    debugPrint("parse_member")
    val kind = current_kind()
    if (kind == SyntaxKind.ObjectKeyword) {
      parse_object_declaration()
    } else if (kind == SyntaxKind.CaseKeyword && source_file.isScala() ||
      kind == SyntaxKind.ClassKeyword) {
      parse_class_declaration()
    } else if (kind == SyntaxKind.DefKeyword) {
      parse_function_declaration()
    } else if (kind == SyntaxKind.EnumKeyword) {
      parseEnumDeclaration()
    } else {
      parse_global_statement()
    }
  }

  def parseMembers(topLevelStatement: bool): Array[MemberSyntax] = {
    debugPrint("parse_members")
    var size = 0
    // TODO: support resizing
    val members = new Array[MemberSyntax](200)

    while (current_kind() != SyntaxKind.EndOfInputToken && current_kind() != SyntaxKind.CloseBraceToken) {
      members(size) = parseMember(topLevelStatement)
      size = size + 1
    }

    val result = new Array[MemberSyntax](size)
    for (i <- 0 to (size - 1)) {
      result(i) = members(i)
    }
    result
  }

  def parse_compilation_unit(): CompilationUnitSyntax = {
    debugPrint("parse_compilation_unit")
    val namespaceDeclaration = parse_namespace_declaration()
    val usingDirectives = parse_usings()
    val members = parseMembers(true)
    val endToken = accept(SyntaxKind.EndOfInputToken)
    new CompilationUnitSyntax(namespaceDeclaration, usingDirectives, members, endToken)
  }
}

case class SyntaxTree(file: SourceFile, root: CompilationUnitSyntax, diagnostics: Array[Diagnostic])