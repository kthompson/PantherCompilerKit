import panther._

case class SyntaxTrivia(kind: int, start: int, text: string) {
  def isStatementTerminator(): bool = kind == SyntaxKind.EndOfLineTrivia
}

case class SyntaxTriviaList() {
  var _items: List[SyntaxTrivia] = List.Nil

  def add(trivia: SyntaxTrivia): unit = {
    _items = List.Cons(trivia, _items)
  }

  def toArray(): Array[SyntaxTrivia] = {
    val result: Array[SyntaxTrivia] = new Array[SyntaxTrivia](_items.length)
    ListModule.fillReverse(result, _items.length - 1, _items)
    result
  }
}

enum SyntaxTokenValue {
  case Boolean(value: bool)
  case String(value: string)
  case Character(value: char)
  case Number(value: int)
  case None

  case Error
}

case class SyntaxToken(
    sourceFile: SourceFile,
    kind: int,
    start: int,
    text: string,
    value: SyntaxTokenValue,
    leading: Array[SyntaxTrivia],
    trailing: Array[SyntaxTrivia]
) {
  val span: TextSpan = new TextSpan(start, text.length)
  val location: TextLocation = new TextLocation(sourceFile, span)

  def isStatementTerminator(): bool =
    if (
      kind == SyntaxKind.EndOfInputToken || kind == SyntaxKind.CloseBraceToken
    ) true
    else _isStatementTerminator(0)

  def _isStatementTerminator(index: int): bool =
    if (index >= trailing.length) {
      false
    } else if (trailing(index).isStatementTerminator()) {
      true
    } else _isStatementTerminator(index + 1)
}

case class SimpleToken(
    kind: int,
    start: int,
    text: string,
    value: SyntaxTokenValue
)

object SyntaxKind {
  // Special Tokens
  val EndOfInputToken = 0
  val IdentifierToken = 1
  val CommaToken = 2

  // Trivia tokens
  val InvalidTokenTrivia = 3
  val EndOfLineTrivia = 4
  val WhitespaceTrivia = 5
  val LineCommentTrivia = 6
  val BlockCommentTrivia = 7

  // Literal tokens
  val NumberToken = 8
  val StringToken = 9
  val CharToken = 10
  val AnnotationToken = 11

  // Keywords
  val FirstKeyword =
    14 // this is a marker and should be set to the first keyword
  val AsKeyword = 14
  val BreakKeyword = 15
  val CaseKeyword = 16
  val ClassKeyword = 17
  val ContinueKeyword = 18
  val DefKeyword = 19
  val EnumKeyword = 20
  val ElseKeyword = 21
  val FalseKeyword = 22
  val ForKeyword = 23
  val IfKeyword = 24
  val InKeyword = 25
  val ImplicitKeyword = 26
  val ImportKeyword = 27
  val MatchKeyword = 28
  val NamespaceKeyword = 29
  val NewKeyword = 30
  val ObjectKeyword = 31
  val OutKeyword = 32
  val OverrideKeyword = 33
  val StaticKeyword = 34
  val ToKeyword = 35
  val TrueKeyword = 36
  val UsingKeyword = 37
  val ValKeyword = 38
  val VarKeyword = 39
  val WhileKeyword = 40
  val LastKeyword = 40 // this is a marker and should be set to the last keyword

  // Operators
  val AmpersandAmpersandToken = 41
  val AmpersandToken = 42
  val BangEqualsToken = 43
  val BangToken = 44
  val CaretToken = 45
  val ColonToken = 46
  val DashToken = 47
  val DotToken = 48
  val EqualsEqualsToken = 49
  val EqualsGreaterThanToken = 50
  val EqualsToken = 51
  val GreaterThanEqualsToken = 52
  val GreaterThanToken = 53
  val LessThanDashToken = 54
  val LessThanEqualsToken = 55
  val LessThanToken = 56
  val PipePipeToken = 57
  val PipeToken = 58
  val PlusToken = 59
  val SlashToken = 60
  val StarToken = 61
  val TildeToken = 62

  // grouping tokens
  val CloseParenToken = 70
  val OpenParenToken = 71
  val OpenBraceToken = 72 // {
  val CloseBraceToken = 73 // }
  val OpenBracketToken = 74 // [
  val CloseBracketToken = 75 // ]

  // Expressions
  val ArrayCreationExpression = 80
  val AssignmentExpression = 81
  val BinaryExpression = 82
  val BlockExpression = 83
  val CallExpression = 84
  val ForExpression = 85
  val GroupExpression = 86
  val IfExpression = 87
  val IndexExpression = 88
  val LiteralExpression = 89
  val MemberAccessExpression = 90
  val NewExpression = 91
  val UnaryExpression = 92
  val UnitExpression = 93
  val WhileExpression = 94

  // Types
  val QualifiedName = 100
  val GenericName = 101
  val IdentifierName = 102
  val SimpleName = 103

  // Statements
  val BreakStatement = 110
  val ContinueStatement = 111
  val ExpressionStatement = 112
  val VariableDeclarationStatement = 113

  //  Nodes
  val ArrayInitializer = 120
  val CompilationUnit = 121
  val FunctionBody = 122
  val Initializer = 123
  val Parameter = 124
  val Template = 125
  val TypeAnnotation = 126
  val TypeArgumentList = 127

  // Members
  val ClassDeclaration = 130
  val FunctionDeclaration = 131
  val ObjectDeclaration = 132
  val GlobalStatement = 133

  // Top level items
  val UsingDirective = 140
  val NamespaceDeclaration = 141
}

object OperatorPrecedence {
  val Lowest = 0
  val Prefix = 9
}

object SyntaxFacts {
  def isBuiltinType(name: string): bool =
    name match {
      case "bool"   => true
      case "int"    => true
      case "string" => true
      case "char"   => true
      case "unit"   => true
      case "any"    => true
      case "never"  => true
      case _        => false
    }

  def getKindName(kind: int): string = {
    if (kind == SyntaxKind.AsKeyword) "AsKeyword"
    else if (kind == SyntaxKind.AmpersandAmpersandToken)
      "AmpersandAmpersandToken"
    else if (kind == SyntaxKind.AmpersandToken) "AmpersandToken"
    else if (kind == SyntaxKind.ArrayCreationExpression)
      "ArrayCreationExpression"
    else if (kind == SyntaxKind.ArrayInitializer) "ArrayInitializer"
    else if (kind == SyntaxKind.AssignmentExpression) "AssignmentExpression"
    else if (kind == SyntaxKind.BangEqualsToken) "BangEqualsToken"
    else if (kind == SyntaxKind.BangToken) "BangToken"
    else if (kind == SyntaxKind.BinaryExpression) "BinaryExpression"
    else if (kind == SyntaxKind.BlockCommentTrivia) "BlockCommentTrivia"
    else if (kind == SyntaxKind.BlockExpression) "BlockExpression"
    else if (kind == SyntaxKind.BreakKeyword) "BreakKeyword"
    else if (kind == SyntaxKind.BreakStatement) "BreakStatement"
    else if (kind == SyntaxKind.CallExpression) "CallExpression"
    else if (kind == SyntaxKind.CaretToken) "CaretToken"
    else if (kind == SyntaxKind.CaseKeyword) "CaseKeyword"
    else if (kind == SyntaxKind.CharToken) "CharToken"
    else if (kind == SyntaxKind.ClassDeclaration) "ClassDeclaration"
    else if (kind == SyntaxKind.ClassKeyword) "ClassKeyword"
    else if (kind == SyntaxKind.CloseBraceToken) "CloseBraceToken"
    else if (kind == SyntaxKind.CloseBracketToken) "CloseBracketToken"
    else if (kind == SyntaxKind.CloseParenToken) "CloseParenToken"
    else if (kind == SyntaxKind.ColonToken) "ColonToken"
    else if (kind == SyntaxKind.CommaToken) "CommaToken"
    else if (kind == SyntaxKind.CompilationUnit) "CompilationUnit"
    else if (kind == SyntaxKind.ContinueKeyword) "ContinueKeyword"
    else if (kind == SyntaxKind.ContinueStatement) "ContinueStatement"
    else if (kind == SyntaxKind.DashToken) "DashToken"
    else if (kind == SyntaxKind.DefKeyword) "DefKeyword"
    else if (kind == SyntaxKind.DotToken) "DotToken"
    else if (kind == SyntaxKind.ElseKeyword) "ElseKeyword"
    else if (kind == SyntaxKind.EndOfInputToken) "EndOfInputToken"
    else if (kind == SyntaxKind.EndOfLineTrivia) "EndOfLineTrivia"
    else if (kind == SyntaxKind.EnumKeyword) "EnumKeyword"
    else if (kind == SyntaxKind.EqualsEqualsToken) "EqualsEqualsToken"
    else if (kind == SyntaxKind.EqualsGreaterThanToken) "EqualsGreaterThanToken"
    else if (kind == SyntaxKind.EqualsToken) "EqualsToken"
    else if (kind == SyntaxKind.ExpressionStatement) "ExpressionStatement"
    else if (kind == SyntaxKind.FalseKeyword) "FalseKeyword"
    else if (kind == SyntaxKind.ForExpression) "ForExpression"
    else if (kind == SyntaxKind.ForKeyword) "ForKeyword"
    else if (kind == SyntaxKind.FunctionBody) "FunctionBody"
    else if (kind == SyntaxKind.FunctionDeclaration) "FunctionDeclaration"
    else if (kind == SyntaxKind.GenericName) "GenericName"
    else if (kind == SyntaxKind.GlobalStatement) "GlobalStatement"
    else if (kind == SyntaxKind.GreaterThanEqualsToken) "GreaterThanEqualsToken"
    else if (kind == SyntaxKind.GreaterThanToken) "GreaterThanToken"
    else if (kind == SyntaxKind.GroupExpression) "GroupExpression"
    else if (kind == SyntaxKind.IdentifierName) "IdentifierName"
    else if (kind == SyntaxKind.IdentifierToken) "IdentifierToken"
    else if (kind == SyntaxKind.IfExpression) "IfExpression"
    else if (kind == SyntaxKind.IfKeyword) "IfKeyword"
    else if (kind == SyntaxKind.ImplicitKeyword) "ImplicitKeyword"
    else if (kind == SyntaxKind.ImportKeyword) "ImportKeyword"
    else if (kind == SyntaxKind.InKeyword) "InKeyword"
    else if (kind == SyntaxKind.IndexExpression) "IndexExpression"
    else if (kind == SyntaxKind.Initializer) "Initializer"
    else if (kind == SyntaxKind.InvalidTokenTrivia) "InvalidTokenTrivia"
    else if (kind == SyntaxKind.LessThanDashToken) "LessThanDashToken"
    else if (kind == SyntaxKind.LessThanEqualsToken) "LessThanEqualsToken"
    else if (kind == SyntaxKind.LessThanToken) "LessThanToken"
    else if (kind == SyntaxKind.LineCommentTrivia) "LineCommentTrivia"
    else if (kind == SyntaxKind.LiteralExpression) "LiteralExpression"
    else if (kind == SyntaxKind.MatchKeyword) "MatchKeyword"
    else if (kind == SyntaxKind.MemberAccessExpression) "MemberAccessExpression"
    else if (kind == SyntaxKind.NamespaceDeclaration) "NamespaceDeclaration"
    else if (kind == SyntaxKind.NamespaceKeyword) "NamespaceKeyword"
    else if (kind == SyntaxKind.NewExpression) "NewExpression"
    else if (kind == SyntaxKind.NewKeyword) "NewKeyword"
    else if (kind == SyntaxKind.NumberToken) "NumberToken"
    else if (kind == SyntaxKind.ObjectDeclaration) "ObjectDeclaration"
    else if (kind == SyntaxKind.ObjectKeyword) "ObjectKeyword"
    else if (kind == SyntaxKind.OpenBraceToken) "OpenBraceToken"
    else if (kind == SyntaxKind.OpenBracketToken) "OpenBracketToken"
    else if (kind == SyntaxKind.OpenParenToken) "OpenParenToken"
    else if (kind == SyntaxKind.OutKeyword) "OutKeyword"
    else if (kind == SyntaxKind.Parameter) "Parameter"
    else if (kind == SyntaxKind.PipePipeToken) "PipePipeToken"
    else if (kind == SyntaxKind.PipeToken) "PipeToken"
    else if (kind == SyntaxKind.PlusToken) "PlusToken"
    else if (kind == SyntaxKind.QualifiedName) "QualifiedName"
    else if (kind == SyntaxKind.SlashToken) "SlashToken"
    else if (kind == SyntaxKind.StarToken) "StarToken"
    else if (kind == SyntaxKind.StaticKeyword) "StaticKeyword"
    else if (kind == SyntaxKind.StringToken) "StringToken"
    else if (kind == SyntaxKind.Template) "Template"
    else if (kind == SyntaxKind.TildeToken) "TildeToken"
    else if (kind == SyntaxKind.ToKeyword) "ToKeyword"
    else if (kind == SyntaxKind.TrueKeyword) "TrueKeyword"
    else if (kind == SyntaxKind.TypeAnnotation) "TypeAnnotation"
    else if (kind == SyntaxKind.TypeArgumentList) "TypeArgumentList"
    else if (kind == SyntaxKind.UnaryExpression) "UnaryExpression"
    else if (kind == SyntaxKind.UnitExpression) "UnitExpression"
    else if (kind == SyntaxKind.UsingDirective) "UsingDirective"
    else if (kind == SyntaxKind.UsingKeyword) "UsingKeyword"
    else if (kind == SyntaxKind.ValKeyword) "ValKeyword"
    else if (kind == SyntaxKind.VarKeyword) "VarKeyword"
    else if (kind == SyntaxKind.VariableDeclarationStatement)
      "VariableDeclarationStatement"
    else if (kind == SyntaxKind.WhileExpression) "WhileExpression"
    else if (kind == SyntaxKind.WhileKeyword) "WhileKeyword"
    else if (kind == SyntaxKind.WhitespaceTrivia) "WhitespaceTrivia"
    else panic("Unknown SyntaxKind: " + kind)
  }

  def isKeywordKind(kind: int): bool =
    kind >= SyntaxKind.FirstKeyword && kind <= SyntaxKind.LastKeyword

  def getKeywordKind(span: string): int = {
    if (span == "as") SyntaxKind.AsKeyword
    else if (span == "break") SyntaxKind.BreakKeyword
    else if (span == "case") SyntaxKind.CaseKeyword
    else if (span == "class") SyntaxKind.ClassKeyword
    else if (span == "continue") SyntaxKind.ContinueKeyword
    else if (span == "def") SyntaxKind.DefKeyword
    else if (span == "else") SyntaxKind.ElseKeyword
    else if (span == "enum") SyntaxKind.EnumKeyword
    else if (span == "false") SyntaxKind.FalseKeyword
    else if (span == "for") SyntaxKind.ForKeyword
    else if (span == "if") SyntaxKind.IfKeyword
    else if (span == "implicit") SyntaxKind.ImplicitKeyword
    else if (span == "import") SyntaxKind.ImportKeyword
    else if (span == "in") SyntaxKind.InKeyword
    else if (span == "match") SyntaxKind.MatchKeyword
    else if (span == "namespace") SyntaxKind.NamespaceKeyword
    else if (span == "object") SyntaxKind.ObjectKeyword
    else if (span == "out") SyntaxKind.OutKeyword
    else if (span == "override") SyntaxKind.OverrideKeyword
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

  def getBinaryOperatorKind(kind: int): BinaryOperatorKind =
    kind match {
      case SyntaxKind.AmpersandAmpersandToken => BinaryOperatorKind.LogicalAnd
      case SyntaxKind.BangEqualsToken         => BinaryOperatorKind.NotEquals
      case SyntaxKind.DashToken               => BinaryOperatorKind.Minus
      case SyntaxKind.EqualsEqualsToken       => BinaryOperatorKind.Equals
      case SyntaxKind.GreaterThanEqualsToken =>
        BinaryOperatorKind.GreaterThanOrEqual
      case SyntaxKind.GreaterThanToken    => BinaryOperatorKind.GreaterThan
      case SyntaxKind.LessThanEqualsToken => BinaryOperatorKind.LessThanOrEqual
      case SyntaxKind.LessThanToken       => BinaryOperatorKind.LessThan
      case SyntaxKind.PipePipeToken       => BinaryOperatorKind.LogicalOr
      case SyntaxKind.PlusToken           => BinaryOperatorKind.Plus
      case SyntaxKind.SlashToken          => BinaryOperatorKind.Divide
      case SyntaxKind.StarToken           => BinaryOperatorKind.Multiply

//        case SyntaxKind.AmpersandToken =>
//        case SyntaxKind.BangToken =>
//        case SyntaxKind.CaretToken =>
//        case SyntaxKind.ColonToken =>
//        case SyntaxKind.DotToken =>
//        case SyntaxKind.EqualsGreaterThanToken =>
//        case SyntaxKind.EqualsToken =>
//        case SyntaxKind.LessThanDashToken =>
//        case SyntaxKind.PipeToken =>
//        case SyntaxKind.TildeToken =>

//        case "%" => BinaryOperatorKind.Modulus
      case _ => BinaryOperatorKind.Error
    }

  def isBinaryOperator(kind: int): bool =
    kind == SyntaxKind.AmpersandAmpersandToken ||
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
      kind == SyntaxKind.StarToken

}
