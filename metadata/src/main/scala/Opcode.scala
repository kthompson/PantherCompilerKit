object Opcode {
  // no operands
  val Nop = 0
  val Ldarg0 = 1 // load argument 0
  val Ldarg1 = 2 // load argument 1
  val Ldarg2 = 3 // load argument 2
  val Ldarg3 = 4 // load argument 3
  val Ldargn = 5 // load argument n

  val Ldloc0 = 6 // load local 0
  val Ldloc1 = 7 // load local 1
  val Ldloc2 = 8 // load local 2
  val Ldloc3 = 9 // load local 3
  val Ldlocn = 10 // load local n

  val Stloc0 = 11 // store local 0
  val Stloc1 = 12 // store local 1
  val Stloc2 = 13 // store local 2
  val Stloc3 = 14 // store local 3
  val Stlocn = 15 // store local n

  val Ret = 20 // return

  // i4 operand
  //  val Ldargn = 20 // load argument n
  //  val Ldlocn = 21 // load local n
  //  val Stlocn = 22 // store local n
  val LdcI4 = 30 // load constant i4
  val Ldstr = 31 // load constant string

  //  // object operand
  //  val Ldfld = 40 // load field of object
  //  val Ldsfld = 41 // load static field of object
  //  val Stfld = 42  // store field of object
  //  val Stsfld = 43 // store static field of object
  //  val Newobj = 44 // new object

  //  // branch
  //  val Br = 50 // branch
  //  val Brfalse = 51 // branch if false
  //  val Brtrue = 52 // branch if true
  //  val Beq = 53  // branch if equal
  //  val Bne = 54  // branch if not equal
  //  val Bge = 55  // branch if greater or equal
  //  val Bgt = 56  // branch if greater
  //  val Ble = 57  // branch if less or equal
  //  val Blt = 58  // branch if less

  // call
  val Call = 60 // call

  //  // array
  //  val Newarr = 70 // new array
  //  val Ldelem = 71 // load element of array
  //  val Stelem = 72 // store element of array
  //  val Ldlen = 73 // load length of array

  // stack
  val Dup = 80 // duplicate top of stack
  val Pop = 81 // pop top of stack
  val Swap = 82 // swap top two of stack
  val Add = 83 // add
  val Sub = 84 // sub
  val Mul = 85 // multiply
  val Div = 86
  val Rem = 87
  val Neg = 88
  val And = 89
  val Or = 90
  val Xor = 91
  val Not = 92
  val Shl = 93
  val Shr = 94

  // compare
  val Ceq = 100
  val Cgt = 101
  val Clt = 102

  // conversion
  val ConvI4 = 110
  val ConvStr = 111

}
