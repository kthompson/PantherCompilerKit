//import panther._
///**
// *
// */
//
////case class Type(
////    var kind: int,
////    var symbol: Option[Symbol],
//////    var intersection: Option[IntersectionType],
////    var primitive: Option[PrimitiveType],
////    var typeVariable: Option[TypeVariable],
////    var function: Option[FunctionType],
////    var array: Option[ArrayType],
////    var option: Option[OptionType],
////    var typeConstructor: Option[TypeConstructor]
////) {
////    var id = -1 // used in type checker
////}
//
//object TypeKind {
////    val Any = 1
////    val Nothing = 2
////    val Intersection = 4
//    val Primitive = 5
////    val TypeVariable = 6
//    val Function = 7
//    val TypeConstructor = 8
//    val Array = 9
//    val Option = 10
//    val Unresolved = 11
//}
//
////case class UnionType(left: Type, right: Type)
//case class ArrayType(inner: Type)
//case class OptionType(inner: Type)
////case class IntersectionType(left: Type, right: Type)
//case class PrimitiveType(name: string)
//case class TypeVariable(name: string, hash: int)
//case class FunctionType(parameters: Array[Type], returnType: Type)
//case class TypeConstructor(name: string, parameters: Array[Type])
////case class RecordType(fields: Array[RecordTypeField])
////case class RecordTypeField(name: string, typ: Type)
//
//object MakeType {
////    val top: Type = new Type(
////        TypeKind.Any,
////        None,
////        None,
////        None,
////        None,
////        None,
////        None,
////        None,
////        None
////    )
////
////    val bottom: Type = new Type(
////        TypeKind.Nothing,
////        None,
////        None,
////        None,
////        None,
////        None,
////        None,
////        None,
////        None
////    )
//
//    def primitive(name: string): Type = new Type(
//        TypeKind.Primitive,
//        None,
//        Some(new PrimitiveType(name) ),
//        None,
//        None,
//        None,
//        None,
//        None
//    )
//
//    def function(symbol: Symbol, parameters: Array[Type], returnType: Type): Type = {
//        new Type(
//            TypeKind.Function,
//            Some(symbol),
//            None,
//            None,
//            Some(new FunctionType(parameters, returnType) ),
//            None,
//            None,
//            None
//        )
//    }
//
//    def typeConstructor(symbol: Symbol, typ: TypeConstructor): Type = {
//        new Type(
//            TypeKind.TypeConstructor,
//            Some(symbol),
//            None,
//            None,
//            None,
//            None,
//            None,
//            Some(typ)
//        )
//    }
//
//    def array(inner: Type): Type =
//        new Type(
//            TypeKind.Array,
//            None,
//            None,
//            None,
//            None,
//            Some(new ArrayType(inner)),
//            None,
//            None
//        )
//
//    def unresolved(symbol: Symbol): Type = new Type(
//        TypeKind.Unresolved,
//        Some(symbol),
//        None,
//        None,
//        None,
//        None,
//        None,
//        None
//    )
//
//    def option(inner: Type): Type = new Type(
//        TypeKind.Option,
//        None,
//        None,
//        None,
//        None,
//        None,
//        Some(new OptionType(inner)),
//        None
//    )
//}