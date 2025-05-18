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
  val GreaterThanGreaterThanToken = 54
  val LessThanDashToken = 55
  val LessThanLessThanToken = 56
  val LessThanEqualsToken = 57
  val LessThanToken = 58
  val PercentToken = 59
  val PipePipeToken = 60
  val PipeToken = 61
  val PlusToken = 62
  val SlashToken = 63
  val StarToken = 64
  val TildeToken = 65

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
