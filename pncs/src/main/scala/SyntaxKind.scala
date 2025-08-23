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
  val IsKeyword = 25
  val InKeyword = 26
  val ImplicitKeyword = 27
  val ImportKeyword = 28
  val MatchKeyword = 29
  val NamespaceKeyword = 30
  val NewKeyword = 31
  val ObjectKeyword = 32
  val OutKeyword = 33
  val OverrideKeyword = 34
  val StaticKeyword = 35
  val ToKeyword = 36
  val TrueKeyword = 37
  val UsingKeyword = 38
  val ValKeyword = 39
  val VarKeyword = 40
  val WhileKeyword = 41
  val LastKeyword = 41 // this is a marker and should be set to the last keyword

  // Operators
  val AmpersandAmpersandToken = 42
  val AmpersandToken = 43
  val BangEqualsToken = 44
  val BangToken = 45
  val CaretToken = 46
  val ColonToken = 47
  val DashToken = 48
  val DotToken = 49
  val EqualsEqualsToken = 50
  val EqualsGreaterThanToken = 51
  val EqualsToken = 52
  val GreaterThanEqualsToken = 53
  val GreaterThanToken = 54
  val GreaterThanGreaterThanToken = 55
  val LessThanDashToken = 56
  val LessThanLessThanToken = 57
  val LessThanEqualsToken = 58
  val LessThanToken = 59
  val PercentToken = 60
  val PipePipeToken = 61
  val PipeToken = 62
  val PlusToken = 63
  val SlashToken = 64
  val StarToken = 65
  val TildeToken = 66

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
  val IsExpression = 89
  val LiteralExpression = 90
  val MemberAccessExpression = 91
  val NewExpression = 92
  val UnaryExpression = 93
  val UnitExpression = 94
  val WhileExpression = 95

  // Types
  val QualifiedName = 101
  val GenericName = 102
  val IdentifierName = 103
  val SimpleName = 104

  // Statements
  val BreakStatement = 111
  val ContinueStatement = 112
  val ExpressionStatement = 113
  val VariableDeclarationStatement = 114

  //  Nodes
  val ArrayInitializer = 121
  val CompilationUnit = 122
  val FunctionBody = 123
  val Initializer = 124
  val Parameter = 125
  val Template = 126
  val TypeAnnotation = 127
  val TypeArgumentList = 128

  // Members
  val ClassDeclaration = 131
  val FunctionDeclaration = 132
  val ObjectDeclaration = 133
  val GlobalStatement = 134

  // Top level items
  val UsingDirective = 141
  val NamespaceDeclaration = 142
}
