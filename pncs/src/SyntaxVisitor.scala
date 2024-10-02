trait SyntaxVisitor[Context, Result] {
  def visitCompilationUnit(node: CompilationUnitSyntax, ctx: Context): Result
  def visitNamespaceDeclaration(node: NamespaceDeclarationSyntax, ctx: Context): Result
  def visitExpressionStatement(node: ExpressionStatementSyntax, ctx: Context): Result
  def visitVariableDeclarationStatement(node: VariableDeclarationStatementSyntax, ctx: Context): Result
  def visitFunctionDeclaration(node: MemberSyntax.FunctionDeclarationSyntax, ctx: Context): Result
  def visitObjectDeclaration(node: MemberSyntax.ObjectDeclarationSyntax, ctx: Context): Result
  def visitClassDeclaration(node: MemberSyntax.ClassDeclarationSyntax, ctx: Context): Result
  def visitUsingDirective(node: UsingDirectiveSyntax, ctx: Context): Result
  def visitParameter(node: ParameterSyntax, ctx: Context): Result
  def visitExpression(node: ExpressionSyntax, ctx: Context): Result
  def visitMember(node: MemberSyntax, ctx: Context): Result
}
