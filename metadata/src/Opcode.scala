object Opcode {
  // no operands
  val Nop = 0
  val Ldarg0 = 1
  val Ldarg1 = 2
  val Ldarg2 = 3
  val Ldarg3 = 4
  val Ldloc0 = 5
  val Ldloc1 = 6
  val Ldloc2 = 7
  val Ldloc3 = 8
  val Stloc0 = 9
  val Stloc1 = 10
  val Stloc2 = 11
  val Stloc3 = 12
  val Ret = 13

  // i4 operand
  val Ldargn = 20
  val Ldlocn = 21
  val Stlocn = 22
  val LdcI4 = 30

  // object operand
  val Ldfld = 40
  val Ldsfld = 41
  val Stfld = 42
  val Stsfld = 43
  val Newobj = 44
}