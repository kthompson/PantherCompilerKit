import panther.*
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class MetadataTests extends AnyFunSpec with Matchers {

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

  describe("Metadata") {
    it("should find type def for methods") {
      point.token shouldBe metadata.findTypeDefForMethod(pointCtor).token
      point.token shouldBe metadata.findTypeDefForMethod(pointGetX).token
      point.token shouldBe metadata.findTypeDefForMethod(pointGetY).token
      point3.token shouldBe metadata.findTypeDefForMethod(point3Ctor).token
      point3.token shouldBe metadata.findTypeDefForMethod(point3GetX).token
      point3.token shouldBe metadata.findTypeDefForMethod(point3GetY).token
      point3.token shouldBe metadata.findTypeDefForMethod(point3GetZ).token
      circle.token shouldBe metadata.findTypeDefForMethod(circleCtor).token
      circle.token shouldBe metadata.findTypeDefForMethod(circleGetCenter).token
      circle.token shouldBe metadata.findTypeDefForMethod(circleGetRadius).token
    }

    it("should get method parameter count") {
      metadata.getMethodParameterCount(pointCtor) shouldBe 2
      metadata.getMethodParameterCount(pointGetX) shouldBe 0
      metadata.getMethodParameterCount(pointGetY) shouldBe 0
      metadata.getMethodParameterCount(point3Ctor) shouldBe 3
      metadata.getMethodParameterCount(point3GetX) shouldBe 0
      metadata.getMethodParameterCount(point3GetY) shouldBe 0
      metadata.getMethodParameterCount(point3GetZ) shouldBe 0
      metadata.getMethodParameterCount(circleCtor) shouldBe 2
      metadata.getMethodParameterCount(circleGetCenter) shouldBe 0
      metadata.getMethodParameterCount(circleGetRadius) shouldBe 0
    }
  }
}
