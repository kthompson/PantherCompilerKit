trait SyntaxVisitor[Context, Result] {
  def visitCompilationUnit(node: CompilationUnitSyntax, ctx: Context): Result
  def visitNamespaceDeclaration(node: NamespaceDeclarationSyntax, ctx: Context): Result
  def visitExpressionStatement(node: StatementSyntax.ExpressionStatement, ctx: Context): Result
  def visitVariableDeclarationStatement(node: StatementSyntax.VariableDeclarationStatement, ctx: Context): Result
  def visitFunctionDeclaration(node: MemberSyntax.FunctionDeclarationSyntax, ctx: Context): Result
  def visitObjectDeclaration(node: MemberSyntax.ObjectDeclarationSyntax, ctx: Context): Result
  def visitClassDeclaration(node: MemberSyntax.ClassDeclarationSyntax, ctx: Context): Result
  def visitUsingDirective(node: UsingDirectiveSyntax, ctx: Context): Result
  def visitParameter(node: ParameterSyntax, ctx: Context): Result
  def visitExpression(node: Expression, ctx: Context): Result
  def visitMember(node: MemberSyntax, ctx: Context): Result
}
