import panther.bool

case class SymbolLinks() {
  var _type: Option[Type] = None

  def set_type(typ: Type): Type = {
    _type = Some(typ)
    typ
  }

  def has_type(): bool = _type.isDefined

  def get_type(): Type = _type.get
  
//  var typeAnnotation: Option[TypeAnnotationSyntax] = None
//  var expression: Option[ExpressionSyntax] = None
//  var function: Option[MemberSyntax.FunctionDeclarationSyntax] = None
//  var parameter: Option[ParameterSyntax] = None
   
}