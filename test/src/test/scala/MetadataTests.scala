import panther.{assert => _, *}
import utest._

object MetadataTests extends TestSuite {

  val metadata: Metadata = Metadata()
  val point: TypeDefToken = metadata.addTypeDef("Point", "", MetadataFlags.None)
  val pointCtor: MethodToken =
    metadata.addMethod(".ctor", MetadataFlags.None, true, 0, 0, 0)
  val pointCtorX: ParamToken = metadata.addParam("x", MetadataFlags.None, 0)
  val pointCtorY: ParamToken = metadata.addParam("y", MetadataFlags.None, 0)

  val pointGetX: MethodToken =
    metadata.addMethod("getX", MetadataFlags.None, true, 0, 0, 0)
  val pointGetY: MethodToken =
    metadata.addMethod("getY", MetadataFlags.None, true, 0, 0, 0)
  val pointX: FieldToken = metadata.addField("x", MetadataFlags.None, 0, 0)
  val pointY: FieldToken = metadata.addField("y", MetadataFlags.None, 1, 0)

  val point3: TypeDefToken =
    metadata.addTypeDef("Point3", "", MetadataFlags.None)
  val point3Ctor: MethodToken =
    metadata.addMethod(".ctor", MetadataFlags.None, true, 0, 0, 0)
  val point3CtorX: ParamToken = metadata.addParam("x", MetadataFlags.None, 0)
  val point3CtorY: ParamToken = metadata.addParam("y", MetadataFlags.None, 0)
  val point3CtorZ: ParamToken = metadata.addParam("z", MetadataFlags.None, 0)
  val point3GetX: MethodToken =
    metadata.addMethod("getX", MetadataFlags.None, true, 0, 0, 0)
  val point3GetY: MethodToken =
    metadata.addMethod("getY", MetadataFlags.None, true, 0, 0, 0)
  val point3GetZ: MethodToken =
    metadata.addMethod("getZ", MetadataFlags.None, true, 0, 0, 0)
  val point3X: FieldToken = metadata.addField("x", MetadataFlags.None, 0, 0)
  val point3Y: FieldToken = metadata.addField("y", MetadataFlags.None, 1, 0)
  val point3Z: FieldToken = metadata.addField("z", MetadataFlags.None, 2, 0)

  val nil: TypeDefToken = metadata.addTypeDef("Nil", "", MetadataFlags.None)

  val circle: TypeDefToken =
    metadata.addTypeDef("Circle", "", MetadataFlags.None)
  val circleCtor: MethodToken =
    metadata.addMethod(".ctor", MetadataFlags.None, true, 0, 0, 0)
  val circleCtorCenter: ParamToken =
    metadata.addParam("center", MetadataFlags.None, 0)
  val circleCtorRadius: ParamToken =
    metadata.addParam("radius", MetadataFlags.None, 0)
  val circleGetCenter: MethodToken =
    metadata.addMethod("getCenter", MetadataFlags.None, true, 0, 0, 0)
  val circleGetRadius: MethodToken =
    metadata.addMethod("getRadius", MetadataFlags.None, true, 0, 0, 0)
  val circleCenter: FieldToken =
    metadata.addField("center", MetadataFlags.None, 0, 0)
  val circleRadius: FieldToken =
    metadata.addField("radius", MetadataFlags.None, 1, 0)

  val tests = Tests {
    test("findTypeDefForMethod") {
      assert(point.token == metadata.findTypeDefForMethod(pointCtor).token)
      assert(point.token == metadata.findTypeDefForMethod(pointGetX).token)
      assert(point.token == metadata.findTypeDefForMethod(pointGetY).token)
      assert(
        point3.token ==
          metadata.findTypeDefForMethod(point3Ctor).token
      )
      assert(
        point3.token ==
          metadata.findTypeDefForMethod(point3GetX).token
      )
      assert(
        point3.token ==
          metadata.findTypeDefForMethod(point3GetY).token
      )
      assert(
        point3.token ==
          metadata.findTypeDefForMethod(point3GetZ).token
      )
      assert(
        circle.token ==
          metadata.findTypeDefForMethod(circleCtor).token
      )
      assert(
        circle.token ==
          metadata.findTypeDefForMethod(circleGetCenter).token
      )
      assert(
        circle.token ==
          metadata.findTypeDefForMethod(circleGetRadius).token
      )
    }

    test("getMethodParameterCount") {
      assert(metadata.getMethodParameterCount(pointCtor) == 2)
      assert(metadata.getMethodParameterCount(pointGetX) == 0)
      assert(metadata.getMethodParameterCount(pointGetY) == 0)
      assert(metadata.getMethodParameterCount(point3Ctor) == 3)
      assert(metadata.getMethodParameterCount(point3GetX) == 0)
      assert(metadata.getMethodParameterCount(point3GetY) == 0)
      assert(metadata.getMethodParameterCount(point3GetZ) == 0)
      assert(metadata.getMethodParameterCount(circleCtor) == 2)
      assert(metadata.getMethodParameterCount(circleGetCenter) == 0)
      assert(metadata.getMethodParameterCount(circleGetRadius) == 0)
    }
  }
}
