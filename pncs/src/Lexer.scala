import panther._

case class Result(is_success: bool, value: any)

object MakeResult {
    val fail = new Result(false, 0)

    def success(value: any) = new Result(true, value)
}

case class Lexer(source_file: SourceFile, diagnostics: DiagnosticBag) {
    var _position: int = 0
    var debug: bool = false

    def peek(position: int): char = source_file.get(position)

    def current(): char = peek(_position)
    def lookahead(): char = peek(_position + 1)

    def next(): unit = {
        if (debug) {
            println("accepting: " + string(current()))
        } else ()

        _position = _position + 1
    }

    def isLetter(c: char): bool = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
    def isDigit(c: char): bool = (c >= '0' && c <= '9')
    def isIdentStart(c: char): bool = isLetter(c) || c == '_'
    def isIdent(c: char): bool = isLetter(c) || isDigit(c) || c == '_'
    def isNewLine(c: char): bool = c == '\n' || c == '\r'
    def isWhitespace(c: char): bool =
        c == '\u0009' || c == '\u000A' || c == '\u000B' || c == '\u000C' || c == '\u000D' || c == '\u0020' ||
        c == '\u0020' || c == '\u0085' || c == '\u00A0' || c == '\u1680' || c == '\u2000' || c == '\u2001' ||
        c == '\u2002' || c == '\u2002' || c == '\u2002' || c == '\u2003' || c == '\u2003' || c == '\u2003' ||
        c == '\u2004' || c == '\u2005' || c == '\u2006' || c == '\u2007' || c == '\u2008' || c == '\u2008' ||
        c == '\u2009' || c == '\u2009' || c == '\u2009' || c == '\u200A' || c == '\u2028' || c == '\u2029' ||
        c == '\u202F' || c == '\u205F' || c == '\u3000'

    def isNonNewLineWhitespace(c: char): bool = !isNewLine(c) && isWhitespace(c)

    def isStartToken(c: char): bool =
        c == '-' || c == ',' || c == ':' || c == '!' || c == '.' || c == '(' || c == ')' || c == '[' || c == ']' ||
        c == '{' || c == '}' || c == '*' || c == '/' || c == '\'' || c == '\"' || c == '&' || c == '+' || c == '<' ||
        c == '=' || c == '>' || c == '|'

    def isInvalidTokenTrivia(c: char): bool = c != '\u0000' && c != '@' && !isStartToken(c) && !isDigit(c) && !isIdentStart(c)

    def hexValue(): int = Hex.fromChar(current())

    def scan() : SyntaxToken = {
        val leading = scan_trivia(true)
        val token = scanSimpleToken()
        val trailing = scan_trivia(true)

        new SyntaxToken(source_file, token.kind, token.start, token.text, token.value, leading, trailing)
    }

    def scanSimpleToken(): SimpleToken = {
        val curr = current()
        val look = lookahead()
        if (curr == '\u0000') {
            new SimpleToken(SyntaxKind.EndOfInputToken, _position, "", MakeSyntaxTokenValue.none())
        } else if (curr == '.') {
            scanSimpleOne(SyntaxKind.DotToken)
        } else if (curr == '(') {
            scanSimpleOne(SyntaxKind.OpenParenToken)
        } else if (curr == ')') {
            scanSimpleOne(SyntaxKind.CloseParenToken)
        } else if (curr == ':') {
            scanSimpleOne(SyntaxKind.ColonToken)
        } else if (curr == ',') {
            scanSimpleOne(SyntaxKind.CommaToken)
        } else if (curr == '<' && look == '=') {
            scanSimpleTwo(SyntaxKind.LessThanEqualsToken)
        } else if (curr == '<' && look == '-') {
            scanSimpleTwo(SyntaxKind.LessThanDashToken)
        } else if (curr == '<') {
            scanSimpleOne(SyntaxKind.LessThanToken)
        } else if (curr == '>' && look == '=') {
            scanSimpleTwo(SyntaxKind.GreaterThanEqualsToken)
        } else if (curr == '>') {
            scanSimpleOne(SyntaxKind.GreaterThanToken)
        } else if (curr == '=' && look == '>') {
            scanSimpleTwo(SyntaxKind.EqualsGreaterThanToken)
        } else if (curr == '=' && look == '=') {
            scanSimpleTwo(SyntaxKind.EqualsEqualsToken)
        } else if (curr == '=') {
            scanSimpleOne(SyntaxKind.EqualsToken)
        } else if (curr == '!' && look == '=') {
            scanSimpleTwo(SyntaxKind.BangEqualsToken)
        } else if (curr == '!') {
            scanSimpleOne(SyntaxKind.BangToken)
        } else if (curr == '{') {
            scanSimpleOne(SyntaxKind.OpenBraceToken)
        } else if (curr == '}') {
            scanSimpleOne(SyntaxKind.CloseBraceToken)
        } else if (curr == '[') {
            scanSimpleOne(SyntaxKind.OpenBracketToken)
        } else if (curr == ']') {
            scanSimpleOne(SyntaxKind.CloseBracketToken)
        } else if (curr == '+') {
            scanSimpleOne(SyntaxKind.PlusToken)
        } else if (curr == '-') {
            scanSimpleOne(SyntaxKind.DashToken)
        } else if (curr == '/') {
            scanSimpleOne(SyntaxKind.SlashToken)
        } else if (curr == '*') {
            scanSimpleOne(SyntaxKind.StarToken)
        } else if (curr == '&' && look == '&') {
            scanSimpleTwo(SyntaxKind.AmpersandAmpersandToken)
        } else if (curr == '&') {
            scanSimpleOne(SyntaxKind.AmpersandToken)
        } else if (curr == '|' && look == '|') {
            scanSimpleTwo(SyntaxKind.PipePipeToken)
        } else if (curr == '|') {
            scanSimpleOne(SyntaxKind.PipeToken)
        } else if (curr == '\"') {
            scanString()
        } else if (curr == '\'') {
            scanChar()
        } else if (curr == '@') {
            scanAnnotation()
        } else if (isDigit(curr)) {
            scanNumber()
        } else if (isIdentStart(curr)) {
            scanIdent()
        } else {
            scanInvalidToken()
        }
    }

    def scanInvalidToken(): SimpleToken = {
        val curr = current()
        diagnostics.reportBadCharacter(new TextLocation(source_file, new TextSpan(_position, 1)), curr)
        val token = new SimpleToken(SyntaxKind.InvalidTokenTrivia, _position, string(curr), MakeSyntaxTokenValue.none())
        next()
        token
    }

    def scanEscapeSequence(): Result = {
        val start = _position
        next() // accept '\'
        val curr = current()
        if (curr == '\'') {
            next()
            MakeResult.success("\'")
        } else if (curr == 'r') {
            next()
            MakeResult.success("\r")
        } else if (curr == 'n') {
            next()
            MakeResult.success("\n")
        } else if (curr == 't') {
            next()
            MakeResult.success("\t")
        } else if (curr == '\\') {
            next()
            MakeResult.success("\\")
        } else if (curr == '"') {
            next()
            MakeResult.success("\"")
        } else if (curr == '0') {
            next()
            MakeResult.success("\u0000")
        } else if (curr == 'u') {
            next()
            scanUtfEscapeSequence(4, start)
        } else if (curr == 'U') {
            next()
            scanUtfEscapeSequence(8, start)
        } else {
            println("scan error - unexpected escape char: " + string(curr))

            MakeResult.fail
        }
    }

    def scanUtfEscapeSequence(digits: int, start: int): Result =
        scanUtfEscapeSequence(digits, start, 0)

    def scanUtfEscapeSequence(digits: int, start: int, value: int): Result = {
        if (digits > 0) {
            val hvalue = hexValue()
            if (hvalue == -1) {
                // TODO: could throw if we are at EOF
                diagnostics.reportInvalidEscapeSequence(new TextLocation(source_file, new TextSpan(start, _position)), current())
                MakeResult.fail
            } else {
                next() // consume the hex digit
                // TODO: support shifts
                // scan_utf_escape_sequence(digits - 1, start, value + (hexValue << (4 * (digits - 1))))
                // TODO: support exponents
                // scan_utf_escape_sequence(digits - 1, start, value + hexValue * (2 ^ 4 * (digits - 1)))
                scanUtfEscapeSequence(digits - 1, start, value + hvalue * Math.pow(2, 4 * (digits - 1)))
            }
        } else {
            MakeResult.success(string(char(value)))
        }
    }

    def scanString(): SimpleToken = {
        val start = _position
        next() // opening "
        _scanString(start, "")
    }

    def _scanString(start: int, value: string): SimpleToken = {
        val curr = current()
        if (curr == '"') {
            next() // end "
            makeStringToken(start, value)
        } else if (curr == '\\') {
            val escape = scanEscapeSequence()
            val newValue = if (escape.is_success) {
                value + string(escape.value)
            } else {
                value
            }
            _scanString(start, newValue)
        } else if (curr == '\r' || curr == '\n' || curr == '\u0000') {
            diagnostics.reportUnterminatedString(new TextLocation(source_file, new TextSpan(start, 1)))
            makeStringToken(start, value)
        } else {
            val newValue = value + string(curr)
            next()
            _scanString(start, newValue)
        }
    }

    def makeSpan(start: int): string = source_file.to_string(start, _position - start)

    def makeStringToken(start: int, value: string): SimpleToken = {
        val span = makeSpan(start)
        new SimpleToken(SyntaxKind.StringToken, start, span, MakeSyntaxTokenValue.string(value))
    }

    def scanChar(): SimpleToken = {
        val start = _position
        next() // open '

        val curr = current()
        val value = if (curr == '\r' || curr == '\n' || curr == '\u0000') {
            diagnostics.reportUnterminatedChar(new TextLocation(source_file, new TextSpan(start, 1)))
            MakeSyntaxTokenValue.none()
        } else if (curr == '\'') {
            diagnostics.reportEmptyCharLiteral(new TextLocation(source_file, new TextSpan(start, 2)))
            MakeSyntaxTokenValue.none()
        } else if (curr == '\\') {
            val result = scanEscapeSequence()
            if (result.is_success) MakeSyntaxTokenValue.char(string(result.value)(0))
            else MakeSyntaxTokenValue.none()
        } else {
            next() // character
            MakeSyntaxTokenValue.char(curr)
        }

        if (current() == '\'') {
            next() // close '
        } else {
            diagnostics.reportUnterminatedChar(new TextLocation(source_file, new TextSpan(start, 1)))
        }

        val span = makeSpan(start)

        new SimpleToken(SyntaxKind.CharToken, start, span, value)
    }

    def scanNumber(): SimpleToken = {
        val start = _position
        var value = hexValue()
        next()
        while (isDigit(current())) {
            value = (value * 10) + hexValue()
            next()
        }
        val span = makeSpan(start)

        new SimpleToken(SyntaxKind.NumberToken, start, span, MakeSyntaxTokenValue.number(value))
    }

    def scanSimpleOne(kind: int): SimpleToken = {
        val start = _position
        val span = source_file.to_string(_position, 1)
        next()
        new SimpleToken(kind, start, span, MakeSyntaxTokenValue.none())
    }

    def scanSimpleTwo(kind: int): SimpleToken = {
        val start = _position
        val span = source_file.to_string(_position, 2)
        next()
        next()
        new SimpleToken(kind, start, span, MakeSyntaxTokenValue.none())
    }

    def scanIdent(): SimpleToken = {
        val start = _position
        next()
        while (isIdent(current())) {
            next()
        }
        val span = makeSpan(start)
        val kind = SyntaxFacts.get_keyword_kind(span)

        new SimpleToken(kind, start, span, MakeSyntaxTokenValue.none())
    }

    def scanAnnotation(): SimpleToken = {
        val start = _position
        next()
        while (isIdent(current())) {
            next()
        }
        val span = makeSpan(start)
        new SimpleToken(SyntaxKind.AnnotationToken, start, span, MakeSyntaxTokenValue.none())
    }

    def scan_trivia(leading: bool): Array[SyntaxTrivia] = {
        val trivia = new SyntaxTriviaList()
        _scanTrivia(leading, trivia)
    }

    def _scanTrivia(leading: bool, trivia: SyntaxTriviaList): Array[SyntaxTrivia] = {
        if (current() == '\u0000') {
            trivia.to_array()
        } else if (isNewLine(current())) {
            trivia.add(scanNewLineTrivia())
            if (!leading) {
                // trailing trivia should always terminate at the end of a line
                trivia.to_array()
            } else {
                // leading trivia will terminate once we find out first non-trivia token
                _scanTrivia(leading, trivia)
            }
        } else if (isNonNewLineWhitespace(current())) {
            trivia.add(scanWhitespaceTrivia())
            _scanTrivia(leading, trivia)
        } else if (current() == '/' && lookahead() == '/') {
            trivia.add(scanLineComment())
            _scanTrivia(leading, trivia)
        } else if (current() == '/' && lookahead() == '*') {
            trivia.add(scanBlockComment())
            _scanTrivia(leading, trivia)
        } else if (isInvalidTokenTrivia(current())) {
            trivia.add(scanInvalidTokenTrivia())
            trivia.to_array()
        } else {
            trivia.to_array()
        }
    }

    def scanInvalidTokenTrivia(): SyntaxTrivia = {
        val token = scanInvalidToken()
        new SyntaxTrivia(token.kind, token.start, token.text)
    }

    def scanLineComment(): SyntaxTrivia = {
        val start = _position
        next() // '/'
        next() // '/'

        while (current() != '\r' && current() != '\n' && current() != '\u0000') {
            next()
        }

        val span = makeSpan(start)

        new SyntaxTrivia(SyntaxKind.LineCommentTrivia, start, span)
    }

    def scanBlockComment(): SyntaxTrivia = {
        val start = _position
        next() // '/'
        next() // '*'

        _scanBlockComment(start)
    }

    def _scanBlockComment(start: int): SyntaxTrivia = {
        val curr = current()
        val look = lookahead()

        if (curr == '\u0000') {
            diagnostics.reportUnterminatedBlockComment(new TextLocation(source_file, new TextSpan(start, _position - start)))
            makeBlockComment(start)
        } else if (curr == '*' && look == '/') {
            next()
            next()
            makeBlockComment(start)
        } else {
            next()
            _scanBlockComment(start)
        }
    }

    def makeBlockComment(start: int): SyntaxTrivia = {
        val span = makeSpan(start)
        new SyntaxTrivia(SyntaxKind.BlockCommentTrivia, start, span)
    }

    def scanNewLineTrivia(): SyntaxTrivia = {
        val start = _position
        next()
        while (isNewLine(current())) {
            next()
        }
        val span = makeSpan(start)

        new SyntaxTrivia(SyntaxKind.EndOfLineTrivia, start, span)
    }

    def scanWhitespaceTrivia(): SyntaxTrivia = {
        val start = _position
        next()
        while (isNonNewLineWhitespace(current())) {
            next()
        }
        val span = makeSpan(start)

        new SyntaxTrivia(SyntaxKind.WhitespaceTrivia, start, span)
    }
}