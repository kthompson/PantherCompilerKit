import panther._
import system.io._

case class Emitter(syntaxTrees: List[SyntaxTree], root: Symbol, /*checker: Checker,*/ outputFile: string) {

  var indent = ""
  var symbolPrefix = ""
  var container: Symbol = root
  var containerHasThis: bool = false

  val nl = "\n"
  val metadata = new Metadata()

  var symbolsToProcessSignatures = new Array[Symbol](0)
  var symbolsToProcessSignaturesCount = 0

  def queueSymbolSignature(symbol: Symbol): unit = {
    // ensure symbolsToProcessSignatures is large enough
    if (symbolsToProcessSignaturesCount == symbolsToProcessSignatures.length) {
      val newLength = if (symbolsToProcessSignatures.length == 0) 4 else symbolsToProcessSignatures.length * 2
      val newArray = new Array[Symbol](newLength)
      for (i <- 0 to (symbolsToProcessSignatures.length - 1)) {
        newArray(i) = symbolsToProcessSignatures(i)
      }
      symbolsToProcessSignatures = newArray
    } else ()

    symbolsToProcessSignatures(symbolsToProcessSignaturesCount) = symbol
    symbolsToProcessSignaturesCount = symbolsToProcessSignaturesCount + 1
  }

//  def emit(): unit = {
//
//    // setup metadata
//    // 1. setup some space for static fields
//    //    a. count the number of static fields, and assign them an index
//    //    b. record number of static fields in the Chunk header
//    // 2. iterate through all the class symbols and record their fields
//    //    a. each field for each class will get an index that we can reference
//    //
//
//    emitMembersMetadata(root.members)
//
//    // build signatures for all symbols
//
//    for (i <- 0 to (symbolsToProcessSignaturesCount - 1)) {
//      val symbol = symbolsToProcessSignatures(i)
//      val links = checker.getSymbolLinks(symbol)
//
//      val sig = getSymbolSignature(symbol)
//      val sigId = metadata.addSignature(sig)
//
//      if (symbol.kind == SymbolKind.Field) {
//        metadata.fields.fields(links.fieldId).fieldSig = sigId
//      } else if (symbol.kind == SymbolKind.Method || symbol.kind == SymbolKind.Constructor) {
//        metadata.methods.methods(links.methodId).methodSig = sigId
//      } else if (symbol.kind == SymbolKind.Parameter) {
//        metadata.params.params(links.paramId).paramSig = sigId
//      } else {
//        ()
//      }
//    }
//
//    val chunk = new Chunk()
//
//    chunk.emitOpcode(Opcode.LdcI4, 123)
//    chunk.emitI4(30, 123)
//    chunk.emitOpcode(Opcode.Ret, 123)
//
//    var offset = 0
//    while (offset < chunk.size) {
//      offset = disassembleInstruction(chunk, offset)
//    }
//  }
//
//  def emitMembersMetadata(members: Scope): unit =
//    emitSymbolsMetadata(members.symbols())
//
//  def emitSymbolsMetadata(symbols: Array[Symbol]): unit =
//    for (i <- 0 to (symbols.length - 1)) {
//      val symbol = symbols(i)
//      emitSymbolMetadata(symbol)
//    }
//
//  def emitSymbolMetadata(symbol: Symbol): unit = {
//    if (symbol.kind == SymbolKind.Class) {
//      emitClassMetadata(symbol)
//    } else if (symbol.kind == SymbolKind.Field) {
//      emitFieldMetadata(symbol)
//    } else if (symbol.kind == SymbolKind.Method || symbol.kind == SymbolKind.Constructor) {
//      emitMethodMetadata(symbol)
//    } else if (symbol.kind == SymbolKind.Parameter) {
//      emitParameterMetadata(symbol)
//    } else {
//      ()
//    }
//  }
//
//  def emitClassMetadata(symbol: Symbol): unit = {
//    val flags =
//      if ((symbol.flags & SymbolFlags.Static) == SymbolFlags.Static) MetadataFlags.Static
//      else MetadataFlags.None
//
//    val links = checker.getSymbolLinks(symbol)
//    links.classId = metadata.addTypeDef(symbol.name, symbol.ns, flags)
//
//    emitMembersMetadata(symbol.members)
//  }
//
//  def emitFieldMetadata(symbol: Symbol): unit = {
//    val flags =
//      if ((symbol.flags & SymbolFlags.Static) == SymbolFlags.Static) MetadataFlags.Static
//      else MetadataFlags.None
//
//    val links = checker.getSymbolLinks(symbol)
//    links.fieldId = metadata.addField(symbol.name, flags, 0)
//
//    queueSymbolSignature(symbol)
//  }

//  private def getSymbolSignature(symbol: Symbol) = {
//    val typ = checker.getTypeOfSymbol(symbol)
//    val sig = new SignatureBuilder()
//    emitTypeSignature(typ, sig)
//    sig.toSignature()
//  }

//  def emitMethodMetadata(symbol: Symbol) = {
//    val flags =
//      if ((symbol.flags & SymbolFlags.Static) == SymbolFlags.Static) MetadataFlags.Static else MetadataFlags.None
//
//    val links = checker.getSymbolLinks(symbol)
//    links.methodId = metadata.addMethod(symbol.name, flags, 0)
//
//    queueSymbolSignature(symbol)
//
//    emitMembersMetadata(symbol.members)
//  }
//
//  def emitParameterMetadata(symbol: Symbol): unit = {
//    val flags = MetadataFlags.None
//
//    val links = checker.getSymbolLinks(symbol)
//    links.paramId = metadata.addParam(symbol.name, flags, 0)
//
//    queueSymbolSignature(symbol)
//  }
//
//  def emitTypeSignature(typ: Type, sig: SignatureBuilder): unit = {
//    if (typ.kind == TypeKind.Primitive) {
//      emitPrimitiveTypeSignature(typ.primitive.get.name, sig)
//    } else if (typ.kind == TypeKind.Function) {
//      sig.writeFunction()
//      emitTypeSignature(typ.function.get.returnType, sig)
//      emitTypeArraySignature(typ.function.get.parameters, sig)
//    } else if (typ.kind == TypeKind.Option) {
//      sig.writeOption()
//      emitTypeSignature(typ.option.get.inner, sig)
//    } else if (typ.kind == TypeKind.Array) {
//      sig.writeArray()
//      emitTypeSignature(typ.array.get.inner, sig)
//    } else if (typ.kind == TypeKind.TypeConstructor) {
//      sig.writeTypeConstructor()
//      val links = checker.getSymbolLinks(typ.symbol.get)
//      sig.write(links.classId)
//      emitTypeArraySignature(typ.typeConstructor.get.parameters, sig)
//    } else {
//      panic("unknown type kind: " + string(typ.kind))
//    }
//  }
//
//  private def emitTypeArraySignature(parameters: Array[Type], sig: SignatureBuilder) = {
//    sig.writeTypeArray(parameters.length)
//    for (i <- 0 to (parameters.length - 1)) {
//      emitTypeSignature(parameters(i), sig)
//    }
//  }

  private def emitPrimitiveTypeSignature(name: string, sig: SignatureBuilder):unit = {
    if (name == "int") {
      sig.writeInt()
    } else if (name == "bool") {
      sig.writeBool()
    } else if (name == "char") {
      sig.writeChar()
    } else if (name == "string") {
      sig.writeString()
    } else if (name == "unit") {
      sig.writeUnit()
    } else if (name == "any") {
      sig.writeAny()
    } else {
      panic("unknown primitive type: " + name)
    }
  }

  def disassembleInstruction(chunk: Chunk, offset: int): int = {
    print(Pad.left(Hex.toString(offset), 4, '0') + " ")
    if (offset > 0 && chunk.readLine(offset) == chunk.readLine(offset - 1)) {
      print("   | ")
    } else {
      val line = chunk.readLine(offset)
      val lineStr = Pad.left(line.toString, 4, ' ')
      print(lineStr + " ")
    }
    val opcode = chunk.readI4(offset)
    if (opcode == Opcode.Ret) {
      simpleInstruction("ret", offset)
    } else if (opcode == Opcode.Nop) {
      simpleInstruction("nop", offset)
//    } else if (opcode == Opcode.Ldarg0) {
//      simpleInstruction("ldarg.0", offset)
//    } else if (opcode == Opcode.Ldarg1) {
//      simpleInstruction("ldarg.1", offset)
//    } else if (opcode == Opcode.Ldarg2) {
//      simpleInstruction("ldarg.2", offset)
//    } else if (opcode == Opcode.Ldarg3) {
//      simpleInstruction("ldarg.3", offset)
//    } else if (opcode == Opcode.Ldloc0) {
//      simpleInstruction("ldloc.0", offset)
//    } else if (opcode == Opcode.Ldloc1) {
//      simpleInstruction("ldloc.1", offset)
//    } else if (opcode == Opcode.Ldloc2) {
//      simpleInstruction("ldloc.2", offset)
//    } else if (opcode == Opcode.Ldloc3) {
//      simpleInstruction("ldloc.3", offset)
//    } else if (opcode == Opcode.Stloc0) {
//      simpleInstruction("stloc.0", offset)
//    } else if (opcode == Opcode.Stloc1) {
//      simpleInstruction("stloc.1", offset)
//    } else if (opcode == Opcode.Stloc2) {
//      simpleInstruction("stloc.2", offset)
//    } else if (opcode == Opcode.Stloc3) {
      simpleInstruction("stloc.3", offset)
    } else if (opcode == Opcode.LdcI4) {
      i4Instruction(chunk, offset, "ldc.i4")
//    } else if (opcode == Opcode.Ldargn) {
//      i4Instruction(chunk, offset, "ldarg")
//    } else if (opcode == Opcode.Ldlocn) {
//      i4Instruction(chunk, offset, "ldloc")
//    } else if (opcode == Opcode.Stlocn) {
//      i4Instruction(chunk, offset, "stloc")
    } else {
      println("Unknown opcode " + string(opcode))
      offset + 1
    }
  }

  private def i4Instruction(chunk: Chunk, offset: int, name: string): int = {
    val value = chunk.readI4(offset + 1)
    println(name + " " + string(value))
    offset + 2
  }

  private def simpleInstruction(name: string, offset: int): int = {
    println(name)
    offset + 1
  }
}