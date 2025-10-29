import panther.*

case class TypeInference(binder: Binder) {

  //
  
  def unify(expected: Type, actual: Type): Either[TextLocation, Unit] = ???
  
  

}

case class TypeInstantiation(binder: Binder) {
  def instantiateClass(typ: Type.GenericClass, typeArguments: List[Type]): Type.InstantiatedGenericClass = ???
  def instantiateAlias(typ: Type.GenericAlias, typeArguments: List[Type]): Type.InstantiatedGenericAlias = ???
  def instantiateFunction(typ: Type.GenericFunction, typeArguments: List[Type]): Type.InstantiatedGenericFunction = ???
}

case class TypeSubstitution(substitutions: Map[Type.GenericTypeParameter, Type]) {
  def substitute(typ: Type): Type = ???
}
