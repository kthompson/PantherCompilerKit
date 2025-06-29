import panther._
import Helpers._
import TestFramework._

object MetadataTests {

  val metadata: Metadata = Metadata()
  val point: TypeDefToken = metadata.addTypeDef("Point", "", MetadataFlags.None)
  val pointCtor: MethodToken =
    metadata.addMethod(".ctor", MetadataFlags.None, 0, 0, 0)
  val pointCtorX: ParamToken = metadata.addParam("x", MetadataFlags.None, 0)
  val pointCtorY: ParamToken = metadata.addParam("y", MetadataFlags.None, 0)

  val pointGetX: MethodToken =
    metadata.addMethod("getX", MetadataFlags.None, 0, 0, 0)
  val pointGetY: MethodToken =
    metadata.addMethod("getY", MetadataFlags.None, 0, 0, 0)
  val pointX: FieldToken = metadata.addField("x", MetadataFlags.None, 0, 0)
  val pointY: FieldToken = metadata.addField("y", MetadataFlags.None, 1, 0)

  val point3: TypeDefToken =
    metadata.addTypeDef("Point3", "", MetadataFlags.None)
  val point3Ctor: MethodToken =
    metadata.addMethod(".ctor", MetadataFlags.None, 0, 0, 0)
  val point3CtorX: ParamToken = metadata.addParam("x", MetadataFlags.None, 0)
  val point3CtorY: ParamToken = metadata.addParam("y", MetadataFlags.None, 0)
  val point3CtorZ: ParamToken = metadata.addParam("z", MetadataFlags.None, 0)
  val point3GetX: MethodToken =
    metadata.addMethod("getX", MetadataFlags.None, 0, 0, 0)
  val point3GetY: MethodToken =
    metadata.addMethod("getY", MetadataFlags.None, 0, 0, 0)
  val point3GetZ: MethodToken =
    metadata.addMethod("getZ", MetadataFlags.None, 0, 0, 0)
  val point3X: FieldToken = metadata.addField("x", MetadataFlags.None, 0, 0)
  val point3Y: FieldToken = metadata.addField("y", MetadataFlags.None, 1, 0)
  val point3Z: FieldToken = metadata.addField("z", MetadataFlags.None, 2, 0)

  val nil: TypeDefToken = metadata.addTypeDef("Nil", "", MetadataFlags.None)

  val circle: TypeDefToken =
    metadata.addTypeDef("Circle", "", MetadataFlags.None)
  val circleCtor: MethodToken =
    metadata.addMethod(".ctor", MetadataFlags.None, 0, 0, 0)
  val circleCtorCenter: ParamToken =
    metadata.addParam("center", MetadataFlags.None, 0)
  val circleCtorRadius: ParamToken =
    metadata.addParam("radius", MetadataFlags.None, 0)
  val circleGetCenter: MethodToken =
    metadata.addMethod("getCenter", MetadataFlags.None, 0, 0, 0)
  val circleGetRadius: MethodToken =
    metadata.addMethod("getRadius", MetadataFlags.None, 0, 0, 0)
  val circleCenter: FieldToken =
    metadata.addField("center", MetadataFlags.None, 0, 0)
  val circleRadius: FieldToken =
    metadata.addField("radius", MetadataFlags.None, 1, 0)

  def run(): unit = {
    suite("Metadata Tests")

    findTypeDefForMethodTests()
    getMethodParameterCountTests()
  }

  def findTypeDefForMethodTests(): unit = {
    test("findTypeDefForMethod")
    Assert.intEqual(point.token, metadata.findTypeDefForMethod(pointCtor).token)
    Assert.intEqual(point.token, metadata.findTypeDefForMethod(pointGetX).token)
    Assert.intEqual(point.token, metadata.findTypeDefForMethod(pointGetY).token)
    Assert.intEqual(
      point3.token,
      metadata.findTypeDefForMethod(point3Ctor).token
    )
    Assert.intEqual(
      point3.token,
      metadata.findTypeDefForMethod(point3GetX).token
    )
    Assert.intEqual(
      point3.token,
      metadata.findTypeDefForMethod(point3GetY).token
    )
    Assert.intEqual(
      point3.token,
      metadata.findTypeDefForMethod(point3GetZ).token
    )
    Assert.intEqual(
      circle.token,
      metadata.findTypeDefForMethod(circleCtor).token
    )
    Assert.intEqual(
      circle.token,
      metadata.findTypeDefForMethod(circleGetCenter).token
    )
    Assert.intEqual(
      circle.token,
      metadata.findTypeDefForMethod(circleGetRadius).token
    )
  }

  def getMethodParameterCountTests(): unit = {
    test("getMethodParameterCount")
    Assert.intEqual(2, metadata.getMethodParameterCount(pointCtor))
    Assert.intEqual(0, metadata.getMethodParameterCount(pointGetX))
    Assert.intEqual(0, metadata.getMethodParameterCount(pointGetY))
    Assert.intEqual(3, metadata.getMethodParameterCount(point3Ctor))
    Assert.intEqual(0, metadata.getMethodParameterCount(point3GetX))
    Assert.intEqual(0, metadata.getMethodParameterCount(point3GetY))
    Assert.intEqual(0, metadata.getMethodParameterCount(point3GetZ))
    Assert.intEqual(2, metadata.getMethodParameterCount(circleCtor))
    Assert.intEqual(0, metadata.getMethodParameterCount(circleGetCenter))
    Assert.intEqual(0, metadata.getMethodParameterCount(circleGetRadius))
  }
}
