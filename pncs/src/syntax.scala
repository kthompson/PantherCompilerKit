import panther._

case class SyntaxTrivia(kind: int, start: int, text: string) {
    def is_statement_terminator(): bool = kind == SyntaxKind.EndOfLineTrivia
}

case class SyntaxTriviaList() {
    var _items: Array[SyntaxTrivia] = new Array[SyntaxTrivia](0)
    var _size: int = 0

    def ensure_capacity(count: int): unit = {
        if (_size + count >= _items.length) {
            var newItems = new Array[SyntaxTrivia]((_size + count) * 2)
            for (i <- 0 to (_size-1)) {
                newItems(i) = _items(i)
            }
            _items = newItems
        } else {
            ()
        }
    }

    def add(trivia: SyntaxTrivia): unit = {
        ensure_capacity(1)
        _items(_size) = trivia
        _size = _size + 1
    }

    def to_array(): Array[SyntaxTrivia] = {
        val newItems = new Array[SyntaxTrivia](_size)
        for (i <- 0 to (_size-1)) {
            newItems(i) = _items(i)
        }
        newItems
    }
}

object SyntaxTokenValueKind {
    val None = 0
    val Boolean = 1
    val String = 2
    val Character = 3
    val Number = 4
}
case class SyntaxTokenValue(kind: int, boolValue: Option[bool], stringValue: Option[string], characterValue: Option[char], numberValue: Option[int])

object MakeSyntaxTokenValue {
    def none() = new SyntaxTokenValue(SyntaxTokenValueKind.None, None, None, None, None)
    def bool(value: bool) = new SyntaxTokenValue(SyntaxTokenValueKind.Boolean, Some(value), None, None, None)
    def string(value: string) = new SyntaxTokenValue(SyntaxTokenValueKind.String, None, Some(value), None, None)
    def char(value: char) = new SyntaxTokenValue(SyntaxTokenValueKind.Character, None, None, Some(value), None)
    def number(value: int) = new SyntaxTokenValue(SyntaxTokenValueKind.Number, None, None, None, Some(value))
}

case class SyntaxToken(source_file: SourceFile, kind: int, start: int, text: string, value: SyntaxTokenValue, leading: Array[SyntaxTrivia], trailing: Array[SyntaxTrivia]) {
    val span: TextSpan = new TextSpan(start, text.length)
    val location: TextLocation = new TextLocation(source_file, span)

    def is_statement_terminator(): bool =
        if (kind == SyntaxKind.EndOfInputToken || kind == SyntaxKind.CloseBraceToken) true
        else is_statement_terminator(0)

    def is_statement_terminator(index: int): bool =
        if (index >= trailing.length) {
            false
        } else if (trailing(index).is_statement_terminator()) {
            true
        } else is_statement_terminator(index + 1)
}

case class SimpleToken(kind: int, start: int, text: string, value: SyntaxTokenValue)

object SyntaxKind {
    // Special Tokens
    val EndOfInputToken         = 0
    val IdentifierToken         = 1
    val CommaToken              = 2

    // Trivia tokens
    val InvalidTokenTrivia      = 3
    val EndOfLineTrivia         = 4
    val WhitespaceTrivia        = 5
    val LineCommentTrivia       = 6
    val BlockCommentTrivia      = 7

    // Literal tokens
    val NumberToken             = 8
    val StringToken             = 9
    val CharToken               = 10
    val AnnotationToken         = 11

    // Keywords
    val BreakKeyword            = 15
    val ClassKeyword            = 16
    val ContinueKeyword         = 17
    val DefKeyword              = 18
    val ElseKeyword             = 19
    val FalseKeyword            = 20
    val ForKeyword              = 21
    val IfKeyword               = 22
    val ImplicitKeyword         = 23
    val NamespaceKeyword        = 24
    val NewKeyword              = 25
    val ObjectKeyword           = 26
    val OverrideKeyword         = 27
    val StaticKeyword           = 28
    val ToKeyword               = 29
    val TrueKeyword             = 30
    val UsingKeyword            = 31
    val ValKeyword              = 32
    val VarKeyword              = 33
    val WhileKeyword            = 34

    // Operators
    val AmpersandAmpersandToken = 41
    val AmpersandToken          = 42
    val BangEqualsToken         = 43
    val BangToken               = 44
    val CaretToken              = 45
    val ColonToken              = 46
    val DashToken               = 47
    val DotToken                = 48
    val EqualsEqualsToken       = 49
    val EqualsToken             = 50
    val GreaterThanEqualsToken  = 51
    val GreaterThanToken        = 52
    val LessThanDashToken       = 53
    val LessThanEqualsToken     = 54
    val LessThanToken           = 55
    val PipePipeToken           = 56
    val PipeToken               = 57
    val PlusToken               = 58
    val SlashToken              = 59
    val StarToken               = 60
    val TildeToken              = 61

    // grouping tokens
    val CloseParenToken         = 70
    val OpenParenToken          = 71
    val OpenBraceToken          = 72 // {
    val CloseBraceToken         = 73 // }
    val OpenBracketToken        = 74 // [
    val CloseBracketToken       = 75 // ]

    // Expressions
    val ArrayCreationExpression = 80
    val AssignmentExpression    = 81
    val BinaryExpression        = 82
    val BlockExpression         = 83
    val CallExpression          = 84
    val ForExpression           = 85
    val GroupExpression         = 86
    val IfExpression            = 87
    val IndexExpression         = 88
    val LiteralExpression       = 89
    val MemberAccessExpression  = 90
    val NewExpression           = 91
    val UnaryExpression         = 92
    val UnitExpression          = 93
    val WhileExpression         = 94

    // Types
    val QualifiedName  = 100
    val GenericName    = 101
    val IdentifierName = 102
    val SimpleName     = 103

    // Statements
    val BreakStatement               = 110
    val ContinueStatement            = 111
    val ExpressionStatement          = 112
    val VariableDeclarationStatement = 113

    //  Nodes
    val ArrayInitializer = 120
    val CompilationUnit  = 121
    val FunctionBody     = 122
    val Initializer      = 123
    val Parameter        = 124
    val Template         = 125
    val TypeAnnotation   = 126
    val TypeArgumentList = 127

    // Members
    val ClassDeclaration    = 130
    val FunctionDeclaration = 131
    val ObjectDeclaration   = 132
    val GlobalStatement     = 133

    // Top level items
    val UsingDirective       = 140
    val NamespaceDeclaration = 141
}

object OperatorPrecedence {
    val Lowest = 0
    val Prefix = 9
}

object SyntaxFacts {
    def get_kind_name(kind: int): string = {
        if (kind == SyntaxKind.EndOfInputToken) "EndOfInputToken"
        else if (kind == SyntaxKind.IdentifierToken) "IdentifierToken"
        else if (kind == SyntaxKind.CommaToken) "CommaToken"
        else if (kind == SyntaxKind.InvalidTokenTrivia) "InvalidTokenTrivia"
        else if (kind == SyntaxKind.EndOfLineTrivia) "EndOfLineTrivia"
        else if (kind == SyntaxKind.WhitespaceTrivia) "WhitespaceTrivia"
        else if (kind == SyntaxKind.LineCommentTrivia) "LineCommentTrivia"
        else if (kind == SyntaxKind.BlockCommentTrivia) "BlockCommentTrivia"
        else if (kind == SyntaxKind.NumberToken) "NumberToken"
        else if (kind == SyntaxKind.StringToken) "StringToken"
        else if (kind == SyntaxKind.CharToken) "CharToken"
        else if (kind == SyntaxKind.BreakKeyword) "BreakKeyword"
        else if (kind == SyntaxKind.ClassKeyword) "ClassKeyword"
        else if (kind == SyntaxKind.ContinueKeyword) "ContinueKeyword"
        else if (kind == SyntaxKind.DefKeyword) "DefKeyword"
        else if (kind == SyntaxKind.ElseKeyword) "ElseKeyword"
        else if (kind == SyntaxKind.FalseKeyword) "FalseKeyword"
        else if (kind == SyntaxKind.ForKeyword) "ForKeyword"
        else if (kind == SyntaxKind.IfKeyword) "IfKeyword"
        else if (kind == SyntaxKind.ImplicitKeyword) "ImplicitKeyword"
        else if (kind == SyntaxKind.NamespaceKeyword) "NamespaceKeyword"
        else if (kind == SyntaxKind.NewKeyword) "NewKeyword"
        else if (kind == SyntaxKind.ObjectKeyword) "ObjectKeyword"
        else if (kind == SyntaxKind.StaticKeyword) "StaticKeyword"
        else if (kind == SyntaxKind.ToKeyword) "ToKeyword"
        else if (kind == SyntaxKind.TrueKeyword) "TrueKeyword"
        else if (kind == SyntaxKind.UsingKeyword) "UsingKeyword"
        else if (kind == SyntaxKind.ValKeyword) "ValKeyword"
        else if (kind == SyntaxKind.VarKeyword) "VarKeyword"
        else if (kind == SyntaxKind.WhileKeyword) "WhileKeyword"
        else if (kind == SyntaxKind.AmpersandAmpersandToken) "AmpersandAmpersandToken"
        else if (kind == SyntaxKind.AmpersandToken) "AmpersandToken"
        else if (kind == SyntaxKind.BangEqualsToken) "BangEqualsToken"
        else if (kind == SyntaxKind.BangToken) "BangToken"
        else if (kind == SyntaxKind.CaretToken) "CaretToken"
        else if (kind == SyntaxKind.ColonToken) "ColonToken"
        else if (kind == SyntaxKind.DashToken) "DashToken"
        else if (kind == SyntaxKind.DotToken) "DotToken"
        else if (kind == SyntaxKind.EqualsEqualsToken) "EqualsEqualsToken"
        else if (kind == SyntaxKind.EqualsToken) "EqualsToken"
        else if (kind == SyntaxKind.GreaterThanEqualsToken) "GreaterThanEqualsToken"
        else if (kind == SyntaxKind.GreaterThanToken) "GreaterThanToken"
        else if (kind == SyntaxKind.LessThanDashToken) "LessThanDashToken"
        else if (kind == SyntaxKind.LessThanEqualsToken) "LessThanEqualsToken"
        else if (kind == SyntaxKind.LessThanToken) "LessThanToken"
        else if (kind == SyntaxKind.PipePipeToken) "PipePipeToken"
        else if (kind == SyntaxKind.PipeToken) "PipeToken"
        else if (kind == SyntaxKind.PlusToken) "PlusToken"
        else if (kind == SyntaxKind.SlashToken) "SlashToken"
        else if (kind == SyntaxKind.StarToken) "StarToken"
        else if (kind == SyntaxKind.TildeToken) "TildeToken"
        else if (kind == SyntaxKind.CloseParenToken) "CloseParenToken"
        else if (kind == SyntaxKind.OpenParenToken) "OpenParenToken"
        else if (kind == SyntaxKind.OpenBraceToken) "OpenBraceToken"
        else if (kind == SyntaxKind.CloseBraceToken) "CloseBraceToken"
        else if (kind == SyntaxKind.OpenBracketToken) "OpenBracketToken"
        else if (kind == SyntaxKind.CloseBracketToken) "CloseBracketToken"
        else if (kind == SyntaxKind.ArrayCreationExpression) "ArrayCreationExpression"
        else if (kind == SyntaxKind.AssignmentExpression) "AssignmentExpression"
        else if (kind == SyntaxKind.BinaryExpression) "BinaryExpression"
        else if (kind == SyntaxKind.BlockExpression) "BlockExpression"
        else if (kind == SyntaxKind.CallExpression) "CallExpression"
        else if (kind == SyntaxKind.ForExpression) "ForExpression"
        else if (kind == SyntaxKind.GroupExpression) "GroupExpression"
        else if (kind == SyntaxKind.IfExpression) "IfExpression"
        else if (kind == SyntaxKind.IndexExpression) "IndexExpression"
        else if (kind == SyntaxKind.LiteralExpression) "LiteralExpression"
        else if (kind == SyntaxKind.MemberAccessExpression) "MemberAccessExpression"
        else if (kind == SyntaxKind.NewExpression) "NewExpression"
        else if (kind == SyntaxKind.UnaryExpression) "UnaryExpression"
        else if (kind == SyntaxKind.UnitExpression) "UnitExpression"
        else if (kind == SyntaxKind.WhileExpression) "WhileExpression"
        else if (kind == SyntaxKind.QualifiedName) "QualifiedName"
        else if (kind == SyntaxKind.GenericName) "GenericName"
        else if (kind == SyntaxKind.IdentifierName) "IdentifierName"
        else if (kind == SyntaxKind.BreakStatement) "BreakStatement"
        else if (kind == SyntaxKind.ContinueStatement) "ContinueStatement"
        else if (kind == SyntaxKind.ExpressionStatement) "ExpressionStatement"
        else if (kind == SyntaxKind.VariableDeclarationStatement) "VariableDeclarationStatement"
        else if (kind == SyntaxKind.ArrayInitializer) "ArrayInitializer"
        else if (kind == SyntaxKind.CompilationUnit) "CompilationUnit"
        else if (kind == SyntaxKind.FunctionBody) "FunctionBody"
        else if (kind == SyntaxKind.Initializer) "Initializer"
        else if (kind == SyntaxKind.Parameter) "Parameter"
        else if (kind == SyntaxKind.Template) "Template"
        else if (kind == SyntaxKind.TypeAnnotation) "TypeAnnotation"
        else if (kind == SyntaxKind.TypeArgumentList) "TypeArgumentList"
        else if (kind == SyntaxKind.ClassDeclaration) "ClassDeclaration"
        else if (kind == SyntaxKind.FunctionDeclaration) "FunctionDeclaration"
        else if (kind == SyntaxKind.ObjectDeclaration) "ObjectDeclaration"
        else if (kind == SyntaxKind.UsingDirective) "UsingDirective"
        else if (kind == SyntaxKind.GlobalStatement) "GlobalStatement"
        else if (kind == SyntaxKind.NamespaceDeclaration) "NamespaceDeclaration"
        else "<invalid>"
    }

    def is_keyword_kind(kind: int): bool =
        kind >= 11 && kind <= 29

    def get_keyword_kind(span: string): int = {
        if (span == "break") SyntaxKind.BreakKeyword
        else if (span == "class") SyntaxKind.ClassKeyword
        else if (span == "continue") SyntaxKind.ContinueKeyword
        else if (span == "def") SyntaxKind.DefKeyword
        else if (span == "else") SyntaxKind.ElseKeyword
        else if (span == "false") SyntaxKind.FalseKeyword
        else if (span == "for") SyntaxKind.ForKeyword
        else if (span == "if") SyntaxKind.IfKeyword
        else if (span == "implicit") SyntaxKind.ImplicitKeyword
        else if (span == "namespace") SyntaxKind.NamespaceKeyword
        else if (span == "object") SyntaxKind.ObjectKeyword
        else if (span == "new") SyntaxKind.NewKeyword
        else if (span == "static") SyntaxKind.StaticKeyword
        else if (span == "to") SyntaxKind.ToKeyword
        else if (span == "true") SyntaxKind.TrueKeyword
        else if (span == "using") SyntaxKind.UsingKeyword
        else if (span == "val") SyntaxKind.ValKeyword
        else if (span == "var") SyntaxKind.VarKeyword
        else if (span == "while") SyntaxKind.WhileKeyword
        else SyntaxKind.IdentifierToken
    }
}