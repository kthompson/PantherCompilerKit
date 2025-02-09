import panther.string
import panther.int
import utest.*

object InferenceTests extends TestSuite {
  val tests = Tests {

    val empty = TextLocationFactory.empty()
    val intType = Type.Named(empty, List.Nil, "int", List.Nil)
    val boolType = Type.Named(empty, List.Nil, "bool", List.Nil)

    def named(name: string): Type = Type.Named(empty, List.Nil, name, List.Nil)

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

    def generic(args: List[GenericTypeParameter], typ: Type): Type =
      Type.GenericType(
        empty,
        args,
        List.Nil,
        typ
      )

    def param(name: string, typ: Type): BoundParameter =
      BoundParameter(name, typ)

    def variable(index: int): Type = Type.Variable(empty, index)

    test("Inference") {
      test("inferFunction") {
        test("<A>(a: A) => int") {
          val g = named("A")
          val f = generic(
            genericArgs(ListModule.one("A"), List.Nil),
            Type.Function(empty, ListModule.one(param("a", g)), intType)
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
          val f = generic(
            genericArgs(ListModule.one("A"), List.Nil),
            Type.Function(
              empty,
              List.Cons(
                param("a", g),
                List.Cons(param("b", g), List.Nil)
              ),
              g
            )
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
