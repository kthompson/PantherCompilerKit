import panther._

import scala.annotation.tailrec

case class TokenList() {
    var _items: Array[SyntaxToken] = new Array[SyntaxToken](0)
    var _size = 0

    def ensure_capacity(count: int): unit = {
        if (_size + count >= _items.length) {
            var newItems = new Array[SyntaxToken]((_size + count) * 2)
            for (i <- 0 to (_size-1)) {
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
        for (i <- 0 to (_size-1)) {
            newItems(i) = _items(i)
        }
        newItems
    }
}

object MakeTokenList {
    def create(scanner: Scanner): Array[SyntaxToken] = {
        val tokenList = new TokenList()
        _build_token_list(scanner, tokenList)
    }

    @tailrec
    def _build_token_list(scanner: Scanner, tokenList: TokenList): Array[SyntaxToken] = {
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
        val scanner = new Scanner(source_file, diagnostics)
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
            next()
        } else ()
    }

    def debugPrint(note: string): unit = {
        val curr = current()
//         println(note + ": " + SyntaxFacts.get_kind_name(curr.kind))
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
        val is_terminating = !in_group && has_statement_terminator(expression)
        // debug_print("is_terminating: " + string(is_terminating))
        is_terminating
    }

    def has_statement_terminator(expression: ExpressionSyntax): bool = {
        val kind = expression.kind
        if (kind == SyntaxKind.IdentifierName) {
            expression.identifierName.get.identifier.is_statement_terminator()
        } else if (kind == SyntaxKind.LiteralExpression) {
            expression.literalExpression.get.token.is_statement_terminator()
        } else if (kind == SyntaxKind.BlockExpression) {
            expression.blockExpression.get.closeBrace.is_statement_terminator()
        } else if (kind == SyntaxKind.NewExpression) {
            expression.newExpression.get.closeParen.is_statement_terminator()
        } else if (kind == SyntaxKind.ArrayCreationExpression) {
            val expr = expression.arrayCreationExpression.get
            if (expr.initializer.isDefined) {
                expr.initializer.get.closeBrace.is_statement_terminator()
            } else {
                expr.closeBracket.is_statement_terminator()
            }
        } else if (kind == SyntaxKind.CallExpression) {
            expression.callExpression.get.closeParen.is_statement_terminator()
        } else if (kind == SyntaxKind.GroupExpression) {
            expression.groupExpression.get.closeParen.is_statement_terminator()
        } else if (kind == SyntaxKind.IndexExpression) {
            expression.indexExpression.get.closeBracket.is_statement_terminator()
        } else if (kind == SyntaxKind.UnitExpression) {
            expression.unitExpression.get.closeParen.is_statement_terminator()
        } else if (kind == SyntaxKind.MemberAccessExpression) {
            expression.memberAccessExpression.get.right.identifier.is_statement_terminator()
        } else if (kind == SyntaxKind.AssignmentExpression) {
            has_statement_terminator(expression.assignmentExpression.get.right)
        } else if (kind == SyntaxKind.BinaryExpression) {
            has_statement_terminator(expression.binaryExpression.get.right)
        } else if (kind == SyntaxKind.IfExpression) {
            has_statement_terminator(expression.ifExpression.get.elseExpr)
        } else if (kind == SyntaxKind.ForExpression) {
            has_statement_terminator(expression.forExpression.get.body)
        } else if (kind == SyntaxKind.UnaryExpression) {
            has_statement_terminator(expression.unaryExpression.get.expression)
        } else if (kind == SyntaxKind.WhileExpression) {
            has_statement_terminator(expression.whileExpression.get.body)
        } else {
            panic("has_statement_terminator: " + SyntaxFacts.get_kind_name(kind))
            false
        }
    }

    def parse_identifier_name(): IdentifierNameSyntax = {
        debugPrint("parse_identifier_name")
        val identifier = accept(SyntaxKind.IdentifierToken)

        new IdentifierNameSyntax(identifier)
    }

    def parse_identifier_name_expression(): ExpressionSyntax = {
        val name = parse_identifier_name()

        new ExpressionSyntax(
            SyntaxKind.IdentifierName,
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            Some(name),
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            None
        )
    }

    def parseQualifiedName(left: NameSyntax): NameSyntax = {
        debugPrint("parse_qualified_name")
        if (left.kind == SyntaxKind.GenericName || current_kind() != SyntaxKind.DotToken) {
            left
        } else {
            val dot = accept()
            val right = parse_simple_name()
            val qn = new QualifiedNameSyntax(left, dot, right)
            parseQualifiedName(
                new NameSyntax(
                    SyntaxKind.QualifiedName,
                    Some(qn),
                    None
                )
            )
        }
    }

    def parseName(): NameSyntax = {
        debugPrint("parse_name")
        val simple_name = parse_simple_name()
        val name = new NameSyntax(
            SyntaxKind.SimpleName,
            None,
            Some(simple_name)
        )

        parseQualifiedName(name)
    }

    def parse_simple_name(): SimpleNameSyntax = {
        debugPrint("parse_simple_name")
        val ident = accept(SyntaxKind.IdentifierToken)

        if (current_kind() == SyntaxKind.LessThanToken) {
            val typeArgumentlist = parse_type_argument_list(false)
            new SimpleNameSyntax(
                SyntaxKind.GenericName,
                Some(
                    new GenericNameSyntax(ident, typeArgumentlist)
                ),
                None
            )
        } else if (source_file.isScala() && current_kind() == SyntaxKind.OpenBracketToken) {
            val typeArgumentlist = parse_type_argument_list(true)
            new SimpleNameSyntax(
                SyntaxKind.GenericName,
                Some(
                    new GenericNameSyntax(ident, typeArgumentlist)
                ),
                None
            )
        } else {
            new SimpleNameSyntax(
                SyntaxKind.IdentifierName,
                None,
                Some(
                    new IdentifierNameSyntax(ident)
                )
            )
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
        val usings = new Array[UsingDirectiveSyntax](5)

        while (current_kind() == SyntaxKind.UsingKeyword) {
            val keyword = accept()
            val name = parseName()

            usings(size) = new UsingDirectiveSyntax(keyword, name)
            size = size + 1
        }

        var result = new Array[UsingDirectiveSyntax](size)
        for (i <- 0 to (size-1)) {
            result(i) = usings(i)
        }
        result
    }

    def parseTemplate(): TemplateSyntax = {
        debugPrint("parse_template")
        val open = accept(SyntaxKind.OpenBraceToken)
        val members = parseMembers()
        val close = accept(SyntaxKind.CloseBraceToken)

        new TemplateSyntax(open, members, close)
    }

    def parse_object_declaration(): MemberSyntax = {
        debugPrint("parse_object_declaration")
        val objectKeyword = accept(SyntaxKind.ObjectKeyword)
        val identifier = accept(SyntaxKind.IdentifierToken)
        val template = parseTemplate()

        new MemberSyntax(
            SyntaxKind.ObjectDeclaration,
            Some(new ObjectDeclarationSyntax(objectKeyword, identifier, template) ),
            None,
            None,
            None
        )
    }

    def parse_class_declaration(): MemberSyntax = {
        debugPrint("parse_class_declaration")
        val keyword = accept()
        val identifier = accept(SyntaxKind.IdentifierToken)
        val open = accept(SyntaxKind.OpenParenToken)
        val parameters = parse_parameter_list()
        val close = accept(SyntaxKind.CloseParenToken)
        val template = if (current_kind() == SyntaxKind.OpenBraceToken) {
             Some(parseTemplate())
        } else {
            None
        }

        val cls = new ClassDeclarationSyntax(keyword, identifier, open, parameters, close, template)
        new MemberSyntax(
            SyntaxKind.ClassDeclaration,
            None,
            Some(cls),
            None,
            None
        )
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
            for (i <- 0 to (size-1)) {
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
        val greaterThan = accept(if(scala) SyntaxKind.CloseBracketToken else SyntaxKind.GreaterThanToken)

        new TypeArgumentListSyntax(lessThan, names, greaterThan)
    }

    def parseTypeAnnotation(): TypeAnnotationSyntax = {
        debugPrint("parse_type_annotation")
        val colon = accept()
        val typ = parseName()

        new TypeAnnotationSyntax(colon, typ)
    }

    def parse_optional_type_annotation(): Option[TypeAnnotationSyntax] = {
        debugPrint("parse_optional_type_annotation")
        if (current_kind() == SyntaxKind.ColonToken) {
            Some(                parseTypeAnnotation()            )
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
            for (i <- 0 to (size-1)) {
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
        // TODO: support resizing
        val statements = new Array[StatementSyntax](20)
        while (current_kind() != SyntaxKind.EndOfInputToken && current_kind() != SyntaxKind.CloseBraceToken) {
            statements(size) = parse_statement()
            size = size + 1
        }

        var result = new Array[StatementSyntax](size)
        for (i <- 0 to (size-1)) {
            result(i) = statements(i)
        }
        result
    }

    def drop_statement(statements: Array[StatementSyntax]): Array[StatementSyntax] = {
        val size = statements.length - 1
        val result = new Array[StatementSyntax](size)
        for (i <- 0 to (size-1)) {
            result(i) = statements(i)
        }
        result
    }

    def parse_block_expression(): ExpressionSyntax = {
        debugPrint("parse_block_expression")
        val openBrace = accept()
        val statements = parse_block_statements()
        val closeBrace = accept(SyntaxKind.CloseBraceToken)

        // convert last statement to expression if possible
        val block =
            if (statements.length > 0) {
                val lastStatement = statements(statements.length - 1)
                if (lastStatement.kind == SyntaxKind.ExpressionStatement) {
                    new BlockExpressionSyntax(
                        openBrace,
                        drop_statement(statements),
                        Some(lastStatement.expressionStatement.get.expression),
                        closeBrace
                    )
                } else {
                    new BlockExpressionSyntax(
                        openBrace,
                        statements,
                        None,
                        closeBrace
                    )
                }
            } else {
                new BlockExpressionSyntax(
                    openBrace,
                    statements,
                    None,
                    closeBrace
                )
            }

        new ExpressionSyntax(
            SyntaxKind.BlockExpression,
            None,
            None,
            None,
            Some(block),
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            None
        )
    }

    def parse_group_or_unit_expression(): ExpressionSyntax = {
        debugPrint("parse_group_or_unit_expression")
        val open = accept()
        if (current_kind() == SyntaxKind.CloseParenToken) {
            val close = accept()
            val unit_expr = new UnitExpressionSyntax(open, close)
            new ExpressionSyntax(
                SyntaxKind.UnitExpression,
                None,
                None,
                None,
                None,
                None,
                None,
                None,
                None,
                None,
                None,
                None,
                None,
                None,
                None,
                Some(unit_expr),
                None
            )
        } else {
            val expr = parse_expression(OperatorPrecedence.Lowest)
            val close2 = accept(SyntaxKind.CloseParenToken)
            val group = new GroupExpressionSyntax(open, expr, close2)

            new ExpressionSyntax(
                SyntaxKind.GroupExpression,
                None,
                None,
                None,
                None,
                None,
                None,
                Some(group),
                None,
                None,
                None,
                None,
                None,
                None,
                None,
                None,
                None
            )
        }
    }

    def parse_literal_expression(): ExpressionSyntax = {
        debugPrint("parse_literal_expression")
        val token = accept()
        val tokenValue = if (token.kind == SyntaxKind.NumberToken) MakeSyntaxTokenValue.number(int(token.text))
                         else if (token.kind == SyntaxKind.CharToken) MakeSyntaxTokenValue.char(token.text(0))
                         else MakeSyntaxTokenValue.string(token.text)
        val literal = new LiteralExpressionSyntax(token, tokenValue)

        new ExpressionSyntax(
            SyntaxKind.LiteralExpression,
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            Some(literal),
            None,
            None,
            None,
            None,
            None
        )
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

        new ExpressionSyntax(
            SyntaxKind.IfExpression,
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            Some(expr),
            None,
            None,
            None,
            None,
            None,
            None,
            None
        )
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

            new ExpressionSyntax(
                SyntaxKind.ArrayCreationExpression,
                Some(arrayExpr),
                None,
                None,
                None,
                None,
                None,
                None,
                None,
                None,
                None,
                None,
                None,
                None,
                None,
                None,
                None
            )
        } else {
            debugPrint("parsing new expression")
            val open = accept(SyntaxKind.OpenParenToken)
            val arguments = parse_expression_list(SyntaxKind.CloseParenToken)
            val close = accept(SyntaxKind.CloseParenToken)

            val expr = new NewExpressionSyntax(keyword, typ, open, arguments, close)

            new ExpressionSyntax(
                SyntaxKind.NewExpression,
                None,
                None,
                None,
                None,
                None,
                None,
                None,
                None,
                None,
                None,
                None,
                None,
                Some(expr),
                None,
                None,
                None
            )
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

        new ExpressionSyntax(
            SyntaxKind.ForExpression,
            None,
            None,
            None,
            None,
            None,
            Some(forExpr),
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            None
        )
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

        new ExpressionSyntax(
            SyntaxKind.WhileExpression,
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            Some(expr)
        )
    }

    def parse_boolean_literal_expression(): ExpressionSyntax = {
        val value = MakeSyntaxTokenValue.bool(current_kind() == SyntaxKind.TrueKeyword)

        val expr = new LiteralExpressionSyntax(accept(), value)

        new ExpressionSyntax(
            SyntaxKind.LiteralExpression,
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            Some(expr),
            None,
            None,
            None,
            None,
            None
        )
    }

    def parse_unary_expression(): ExpressionSyntax = {
        val unary_op = accept()
        val expr = parse_expression(OperatorPrecedence.Lowest)

        val unaryExpr = new UnaryExpressionSyntax(unary_op, expr)

        new ExpressionSyntax(
            SyntaxKind.UnaryExpression,
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            Some(unaryExpr),
            None,
            None
        )
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
            todo("parse_prefix_expression")

            new ExpressionSyntax(
                0,
                None,
                None,
                None,
                None,
                None,
                None,
                None,
                None,
                None,
                None,
                None,
                None,
                None,
                None,
                None,
                None
            )
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
            for (i <- 0 to (size-1)) {
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
        new ExpressionSyntax(
            SyntaxKind.CallExpression,
            None,
            None,
            None,
            None,
            Some(callExpression),
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            None
        )
    }

    def parse_binary_expression(left: ExpressionSyntax): ExpressionSyntax = {
        debugPrint("parse_binary_expression")
        val precedence = current_precedence()
        val operator = accept()
        var right = parse_expression(precedence)

        val expr = new BinaryExpressionSyntax(left, operator, right)

        new ExpressionSyntax(
            SyntaxKind.BinaryExpression,
            None,
            None,
            Some(expr),
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            None
        )
    }

    def parse_member_access_expression(left: ExpressionSyntax): ExpressionSyntax = {
        val dot = accept()
        val right = parse_identifier_name()

        val expr = new MemberAccessExpressionSyntax(left, dot, right)

        new ExpressionSyntax(
            SyntaxKind.MemberAccessExpression,
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            Some(expr),
            None,
            None,
            None,
            None
        )
    }

    def parse_index_expression(left: ExpressionSyntax): ExpressionSyntax  = {
        val openBracket = accept()
        val index = parse_expression(OperatorPrecedence.Lowest)
        val closeBracket = accept(SyntaxKind.CloseBracketToken)

        val expr = new IndexExpressionSyntax(left, openBracket, index, closeBracket)

        new ExpressionSyntax(
            SyntaxKind.IndexExpression,
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            Some(expr),
            None,
            None,
            None,
            None,
            None,
            None
        )
    }

    def parse_assignment_expression(left: ExpressionSyntax): ExpressionSyntax = {
        val equals = accept()
        val right = parse_expression(OperatorPrecedence.Lowest)

        val expr = new AssignmentExpressionSyntax(left, equals, right)

        new ExpressionSyntax(
            SyntaxKind.AssignmentExpression,
            None,
            Some(expr),
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            None
        )
    }

    def parse_infix_expression(left: ExpressionSyntax, in_group: bool, precedence: int): ExpressionSyntax = {
        debugPrint("parse_infix_expression")
        val kind = current_kind()
        if (is_terminating_line(in_group, left)) {
            left
        } else if (precedence >= current_precedence()) {
            // println("terminating expression due to operator precedence. kind: " + SyntaxFacts.get_kind_name(kind) + ", current: " + string(current_precedence()) + ", precedence: " + string(precedence))
            left
        } else {
            val expr =
                if (kind == SyntaxKind.OpenParenToken) {
                    parse_call_expression(left)
                } else if (kind == SyntaxKind.AmpersandAmpersandToken ||
                    kind == SyntaxKind.AmpersandToken ||
                    kind == SyntaxKind.BangEqualsToken ||
                    kind == SyntaxKind.CaretToken ||
                    kind == SyntaxKind.DashToken ||
                    kind == SyntaxKind.EqualsEqualsToken ||
                    kind == SyntaxKind.GreaterThanEqualsToken ||
                    kind == SyntaxKind.GreaterThanToken ||
                    kind == SyntaxKind.LessThanEqualsToken ||
                    kind == SyntaxKind.LessThanToken ||
                    kind == SyntaxKind.PipePipeToken ||
                    kind == SyntaxKind.PipeToken ||
                    kind == SyntaxKind.PlusToken ||
                    kind == SyntaxKind.SlashToken ||
                    kind == SyntaxKind.StarToken) {
                    parse_binary_expression(left)
                } else if (kind == SyntaxKind.DotToken) {
                    parse_member_access_expression(left)
                } else if (kind == SyntaxKind.OpenBracketToken) {
                    parse_index_expression(left)
                } else if (kind == SyntaxKind.EqualsToken) {
                    parse_assignment_expression(left)
                } else {
                    todo("infix")
                    left
                }
            parse_infix_expression(expr, in_group, precedence)
        }
    }

    def todo(note: string): unit = {
        val curr = current()
        AstPrinter.printTokenInfo(curr)
        panic(note + ": " + SyntaxFacts.get_kind_name(curr.kind))
    }

    def parse_function_body(): Option[FunctionBodySyntax] = {
        debugPrint("parse_function_body")
        if (current_kind() == SyntaxKind.EqualsToken) {
            val equal = accept()
            val expr = parse_expression(OperatorPrecedence.Lowest)
            Some(                new FunctionBodySyntax(equal, expr)    )
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
        val typeAnnotation = parse_optional_type_annotation()
        val body = parse_function_body()


        val function = new FunctionDeclarationSyntax(
            defKeyword,
            identifier,
            openParenToken,
            parameters,
            closeParenToken,
            typeAnnotation,
            body
        )
        new MemberSyntax(
            SyntaxKind.FunctionDeclaration,
            None,
            None,
            Some(function),
            None
        )
    }

    def parse_break_statement(): StatementSyntax = {
        debugPrint("parse_break_statement")

        val keyword = accept()

        val stmt = new BreakStatementSyntax(keyword)

        new StatementSyntax(
            SyntaxKind.BreakStatement,
            None,
            Some(stmt),
            None,
            None
        )
    }

    def parse_continue_statement(): StatementSyntax = {
        debugPrint("parse_continue_statement")

        val keyword = accept()

        val stmt = new ContinueStatementSyntax(keyword)

        new StatementSyntax(
            SyntaxKind.ContinueStatement,
            None,
            None,
            Some(stmt),
            None
        )
    }

    def parse_expression_statement(): StatementSyntax = {
        debugPrint("parse_expression_statement")
        val expr = parse_expression(OperatorPrecedence.Lowest)
        // assert_statement_terminator(expr)
        new StatementSyntax(
            SyntaxKind.ExpressionStatement,
            None,
            None,
            None,
            Some(new ExpressionStatementSyntax(expr))
        )
    }

    def parse_variable_declaration_statement(): StatementSyntax = {
        debugPrint("parse_variable_declaration_statement")
        val keyword = accept()
        val ident = accept(SyntaxKind.IdentifierToken)
        val typeAnnotation = parse_optional_type_annotation()
        val eq = accept(SyntaxKind.EqualsToken)
        val expr = parse_expression(OperatorPrecedence.Lowest)

        val decl = new VariableDeclarationStatementSyntax(
            keyword,
            ident,
            typeAnnotation,
            eq,
            expr
        )

        new StatementSyntax(
            SyntaxKind.VariableDeclarationStatement,
            Some(decl),
            None,
            None,
            None
        )
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

    def parse_global_statement(): GlobalStatementSyntax = {
        debugPrint("parse_global_statement")
        new GlobalStatementSyntax(parse_statement())
    }

    def parse_global_statement_member(): MemberSyntax = {
        debugPrint("parse_global_statement_member")
        val stmt = parse_global_statement()
        new MemberSyntax(
            SyntaxKind.GlobalStatement,
            None,
            None,
            None,
            Some(stmt)
        )
    }

    def parse_member(): MemberSyntax = {
        debugPrint("parse_member")
        val kind = current_kind()
        if (kind == SyntaxKind.ObjectKeyword) {
            parse_object_declaration()
        } else if (kind == SyntaxKind.ClassKeyword) {
            parse_class_declaration()
        } else if (kind == SyntaxKind.DefKeyword) {
            parse_function_declaration()
        } else {
            parse_global_statement_member()
        }
    }

    def parseMembers(): Array[MemberSyntax] = {
        debugPrint("parse_members")
        var size = 0
        // TODO: support resizing
        val members = new Array[MemberSyntax](100)

        while (current_kind() != SyntaxKind.EndOfInputToken && current_kind() != SyntaxKind.CloseBraceToken) {
            members(size) = parse_member()
            size = size + 1
        }

        val result = new Array[MemberSyntax](size)
        for (i <- 0 to (size-1)) {
            result(i) = members(i)
        }
        result
    }

    def parse_compilation_unit(): CompilationUnitSyntax = {
        debugPrint("parse_compilation_unit")
        val namespaceDeclaration = parse_namespace_declaration()
        val usingDirectives = parse_usings()
        val members = parseMembers()
        val endToken = accept(SyntaxKind.EndOfInputToken)
        new CompilationUnitSyntax(namespaceDeclaration, usingDirectives, members, endToken)
    }
}

object MakeSyntaxTree {
    def parse_source_file(file: SourceFile): SyntaxTree = {
        val diagnosticBag = new DiagnosticBag()
        val parser = new Parser(file, diagnosticBag)
        val root = parser.parse_compilation_unit()

        new SyntaxTree(file, root, diagnosticBag.diagnostics())
    }

    def parse_file(filename: string): SyntaxTree =
        parse_source_file(MakeSourceFile.from_file(filename))

    def parse_content(content: string): SyntaxTree =
        parse_source_file(MakeSourceFile.from_content(content))
}

case class SyntaxTree(file: SourceFile, root: CompilationUnitSyntax, diagnostics: Array[Diagnostic])