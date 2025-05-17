import panther.{int, panic, string}
import utest._

object InferenceTests extends TestSuite {
  val tests = Tests {

    val empty = TextLocationFactory.empty()
    val intType = Type.Class(empty, List.Nil, "int", List.Nil, Option.None)
    val boolType = Type.Class(empty, List.Nil, "bool", List.Nil, Option.None)

    def named(name: string): Type =
      Type.Class(empty, List.Nil, name, List.Nil, Option.None)

    def genericArg(name: string): GenericTypeParameter = GenericTypeParameter(
      empty,
      name,
      Variance.Invariant,
      Option.None
    )

    def genericArgs(
        names: List[string],
        args: List[GenericTypeParameter]
    ): List[GenericTypeParameter] =
      names match {
        case List.Nil => args
        case List.Cons(name, tail) =>
          genericArgs(tail, List.Cons(genericArg(name), args))
      }

    def genericFn(
        args: List[GenericTypeParameter],
        parameters: List[BoundParameter],
        returnType: Type
    ): Type =
      Type.GenericFunction(
        empty,
        args,
        List.Nil,
        parameters,
        returnType
      )

    def param(name: string, typ: Type): BoundParameter = {
      panic("FIXME")
//      BoundParameter(name, typ)
    }

    def variable(index: int): Type = Type.Variable(empty, index)

    test("Inference") {
      test("inferFunction") {
        test("<A>(a: A) => int") {
          val g = named("A")
          val f = genericFn(
            genericArgs(ListModule.one("A"), List.Nil),
            ListModule.one(param("a", g)),
            intType
          )

          val diagnostics = new DiagnosticBag()
          val inference = new Inference(diagnostics)

          inference.instantiate(DictionaryModule.empty(), f) match {
            case function: Type.Function =>
              val a = function.parameters.getUnsafe(0).typ
              val f = inference.inferFunction(
                ListModule.one(
                  Constraint.Equality(a, intType)
                ),
                function
              )

              assert(
                f == Type.Function(
                  empty,
                  ListModule.one(param("a", intType)),
                  intType
                )
              )

            case _ => ???
          }
        }

        test("<A>(a: A, b: A) => A") {
          val g = named("A")
          val f = genericFn(
            genericArgs(ListModule.one("A"), List.Nil),
            List.Cons(
              param("a", g),
              List.Cons(param("b", g), List.Nil)
            ),
            g
          )

          val diagnostics = new DiagnosticBag()
          val inference = new Inference(diagnostics)

          inference.instantiate(DictionaryModule.empty(), f) match {
            case function: Type.Function =>
              val a = function.parameters.getUnsafe(0).typ
              val b = function.parameters.getUnsafe(1).typ
              val f = inference.inferFunction(
                List.Cons(
                  Constraint.Equality(a, intType),
                  List.Cons(
                    Constraint.Equality(b, intType),
                    List.Nil
                  )
                ),
                function
              )

              assert(
                f == Type.Function(
                  empty,
                  List.Cons(
                    param("a", intType),
                    List.Cons(
                      param("b", intType),
                      List.Nil
                    )
                  ),
                  intType
                )
              )

            case _ => ???
          }
        }
      }
    }
  }
}
