import panther._
import system.io._

case class EmitResult(
    chunk: Chunk,
    metadata: Metadata,
    entry: Option[MethodToken]
)

case class EmitContext(
    chunk: Chunk,
    method: MethodMetadata,
    // map of local symbols to their method local index
    var locals: Dictionary[Symbol, int]
) {

  /** A map of labels to their instruction index.
    *
    * This is used to resolve branch targets during emission.
    *
    * If the branch target is not known at the time of emission, a Patch is
    * created and added to the patches list.
    */
  var labels = DictionaryModule.empty[string, int]()
  var patches: List[Patch] = List.Nil

  val startAddress: int = chunk.size

  def getBranchTarget(label: Label): int = {
    labels.get(label.name) match {
      case Option.None =>
        // label not found, create a patch
        patches = List.Cons(Patch(chunk.size, label), patches)
        0 // return a filler value since target is not known yet
      case Option.Some(value) => value
    }
  }

  def saveLabel(label: Label): unit = {
    if (labels.contains(label.name)) {
      panic("BUG!!! Duplicate label " + label.name)
    } else {
      labels = labels.put(label.name, chunk.size)
    }
  }

  def applyPatches(): unit = {
    patches match {
      case List.Nil =>
      case List.Cons(patch, tail) =>
        patches = tail
        labels.get(patch.target.name) match {
          case Option.None =>
            panic(
              "failed to patch label " + patch.target.name + " at " + patch.instructionIndex
            )
          case Option.Some(value) =>
            // patch the instruction at the given index with the target address
            chunk.patch(patch.instructionIndex, value)
        }

        applyPatches() // recursively apply remaining patches
    }
  }

  def getLocalIndex(symbol: Symbol): int = {
    locals.get(symbol) match {
      case Option.None        => _addLocal(symbol)
      case Option.Some(value) => value
    }
  }

  def _addLocal(symbol: Symbol): int = {
    val index = method.locals
    locals = locals.put(symbol, index)
    method.locals = method.locals + 1
    index
  }
}

/** Represents a patch to be applied to a branch instruction once we know the
  * target label's instruction index.
  *
  * @param instructionIndex
  * @param target
  */
case class Patch(
    instructionIndex: int,
    target: Label
)

case class Emitter(
    syntaxTrees: List[SyntaxTree],
    root: Symbol,
    binder: Binder,
    assembly: LoweredAssembly
) {

  var indent = ""
  var symbolPrefix = ""
  var container: Symbol = root
  var containerHasThis: bool = false

  val nl = "\n"
  val metadata = new Metadata()
  val chunk = new Chunk()

  var typeTokens: Dictionary[Symbol, TypeDefToken] =
    DictionaryModule.empty[Symbol, TypeDefToken]()

  var methodTokens: Dictionary[Symbol, MethodToken] =
    DictionaryModule.empty[Symbol, MethodToken]()

  var fieldTokens: Dictionary[Symbol, FieldToken] =
    DictionaryModule.empty[Symbol, FieldToken]()

  var paramTokens: Dictionary[Symbol, ParamToken] =
    DictionaryModule.empty[Symbol, ParamToken]()

  var symbolsToProcessSignatures: List[Symbol] = List.Nil

  def queueSymbolSignature(symbol: Symbol): unit = {
    symbolsToProcessSignatures = List.Cons(symbol, symbolsToProcessSignatures)
  }

  def emit(): EmitResult = {
    // populate symbol metadata
    emitSymbolMetadata(root)

    // update field, method, and parameter tokens with their signatures
    buildSignatures(symbolsToProcessSignatures)

    // update methods local count, body address within chunk
    emitMethodBodies(methodTokens)

    val entryMethod = assembly.entryPoint match {
      case Option.None => Option.None
      case Option.Some(entry) =>
        methodTokens.get(entry) match {
          case Option.None =>
            println(entry.toString())
            ???
          case Option.Some(value) => Option.Some(value)
        }
    }

    EmitResult(chunk, metadata, entryMethod)
  }

  def emitMethodBodies(value: Dictionary[Symbol, MethodToken]): unit = {
    value.list match {
      case List.Nil => ()
      case List.Cons(KeyValue(key, value), tail) =>
        val context = EmitContext(
          chunk,
          metadata.methods.methods(value.token),
          DictionaryModule.empty[Symbol, int]()
        )
        emitMethod(key, context)
        context.applyPatches()

        emitMethodBodies(Dictionary(tail))
    }
  }

  def buildSignature(symbol: Symbol): unit = {
    val sig = getSymbolSignature(symbol)
    val sigId = metadata.addSignature(sig)

    if (symbol.kind == SymbolKind.Field) {
      fieldTokens.get(symbol) match {
        case Option.None => ???
        case Option.Some(value) =>
          metadata.fields.fields(value.token).fieldSig = sigId
      }
    } else if (
      symbol.kind == SymbolKind.Method || symbol.kind == SymbolKind.Constructor
    ) {
      methodTokens.get(symbol) match {
        case Option.None =>
          println("buildSignature: no method token for " + symbol)
          ???
        case Option.Some(value) =>
          metadata.methods.methods(value.token).methodSig = sigId
      }
    } else if (symbol.kind == SymbolKind.Parameter) {
      paramTokens.get(symbol) match {
        case Option.None => ???
        case Option.Some(value) =>
          metadata.params.params(value.token).paramSig = sigId
      }
    } else {
      ???
    }
  }

  def buildSignatures(symbols: List[Symbol]): unit = {
    symbols match {
      case List.Nil =>
      case List.Cons(head, tail) =>
        buildSignature(head)
        buildSignatures(tail)
    }
  }

  //  def emitMethodSymbol(symbol: Symbol, chunk: Chunk): unit = {
  //    val links = checker.getSymbolLinks(symbol)
  //    val sig = getSymbolSignature(symbol)
  //    val sigId = metadata.addSignature(sig)
  //
  //    links.methodId = metadata.addMethod(
  //      symbol.name,
  //      MetadataFlags.None,
  //      sigId
  //    )

  //  def emitMembersMetadata(members: Scope): unit =
  //    emitSymbolsMetadata(members.symbols())

  def emitMethod(symbol: Symbol, context: EmitContext): bool = {
    assembly.functionBodies.get(symbol) match {
      case Option.None => false
      case Option.Some(value) =>
        emitBlock(value, context)
        chunk.emitOpcode(Opcode.Ret, symbol.location.endLine)

        context.method.address = context.startAddress
        true
    }
  }

  def emitBlock(block: LoweredBlock, context: EmitContext): unit = {
    emitStatements(block.statements, context)
    emitExpression(block.expression, context)
  }

  def emitExpression(expr: LoweredExpression, context: EmitContext): unit = {
    expr match {
      case LoweredExpression.Error => panic("emitExpression: Error")
      case value: LoweredExpression.BinaryExpression =>
        emitBinaryExpression(value, context)
      case value: LoweredExpression.BooleanLiteral =>
        emitBooleanLiteral(value, context)
      case value: LoweredExpression.Call =>
        emitCallExpression(value, context)
      case value: LoweredExpression.Cast =>
        emitCastExpression(value, context)
      case value: LoweredExpression.CharacterLiteral =>
        emitCharacterLiteral(value, context)
      case value: LoweredExpression.IntegerLiteral =>
        emitIntLiteral(value, context)
      case value: LoweredExpression.MemberAccess =>
        emitMemberAccess(value, context)
      case value: LoweredExpression.New =>
        emitNewExpression(value, context)
      case value: LoweredExpression.StringLiteral =>
        emitStringLiteral(value, context)
      case value: LoweredExpression.Unary =>
        emitUnaryExpression(value, context)
      case value: LoweredExpression.Variable =>
        emitVariable(value, context)
      case LoweredExpression.Unit =>
        emitUnitExpression(context)
    }
  }

  def emitAssignmentStatement(
      expr: LoweredStatement.Assignment,
      context: EmitContext
  ): unit = {
    emitExpression(expr.expression, context)
    val index = context.getLocalIndex(expr.variable)
    if (index < 4) {
      chunk.emitOpcode(Opcode.Stloc0 + index, expr.location.startLine)
    } else {
      chunk.emitOpcode(Opcode.Stlocn, expr.location.startLine)
      chunk.emitI4(index, expr.location.startLine)
    }
  }

  def emitBinaryExpression(
      expr: LoweredExpression.BinaryExpression,
      context: EmitContext
  ): unit = {
    emitExpression(expr.left, context)
    emitExpression(expr.right, context)
    expr.operator match {
      case BinaryOperatorKind.Plus =>
        chunk.emitOpcode(Opcode.Add, expr.location.startLine)

      case BinaryOperatorKind.Minus =>
        chunk.emitOpcode(Opcode.Sub, expr.location.startLine)

      case BinaryOperatorKind.Multiply =>
        chunk.emitOpcode(Opcode.Mul, expr.location.startLine)

      case BinaryOperatorKind.Divide =>
        chunk.emitOpcode(Opcode.Div, expr.location.startLine)

      case BinaryOperatorKind.Modulus =>
        chunk.emitOpcode(Opcode.Rem, expr.location.startLine)

      case BinaryOperatorKind.Equals =>
        chunk.emitOpcode(Opcode.Ceq, expr.location.startLine)

      case BinaryOperatorKind.NotEquals =>
        chunk.emitOpcode(Opcode.Ceq, expr.location.startLine)

        emitLoadConstant(0, expr.location.startLine)
        chunk.emitOpcode(Opcode.Ceq, expr.location.startLine)

      case BinaryOperatorKind.LessThan =>
        chunk.emitOpcode(Opcode.Clt, expr.location.startLine)

      case BinaryOperatorKind.LessThanOrEqual =>
        chunk.emitOpcode(Opcode.Cgt, expr.location.startLine)
        emitLoadConstant(0, expr.location.startLine)
        chunk.emitOpcode(Opcode.Ceq, expr.location.startLine)

      case BinaryOperatorKind.GreaterThan =>
        chunk.emitOpcode(Opcode.Cgt, expr.location.startLine)

      case BinaryOperatorKind.GreaterThanOrEqual =>
        chunk.emitOpcode(Opcode.Clt, expr.location.startLine)
        emitLoadConstant(0, expr.location.startLine)
        chunk.emitOpcode(Opcode.Ceq, expr.location.startLine)

      case BinaryOperatorKind.LogicalAnd =>
        chunk.emitOpcode(Opcode.And, expr.location.startLine)

      case BinaryOperatorKind.LogicalOr =>
        chunk.emitOpcode(Opcode.Or, expr.location.startLine)

      case BinaryOperatorKind.BitwiseAnd =>
        chunk.emitOpcode(Opcode.And, expr.location.startLine)

      case BinaryOperatorKind.BitwiseOr =>
        chunk.emitOpcode(Opcode.Or, expr.location.startLine)

      case BinaryOperatorKind.BitwiseXor =>
        chunk.emitOpcode(Opcode.Xor, expr.location.startLine)

      case BinaryOperatorKind.ShiftLeft =>
        chunk.emitOpcode(Opcode.Shl, expr.location.startLine)

      case BinaryOperatorKind.ShiftRight =>
        chunk.emitOpcode(Opcode.Shr, expr.location.startLine)

      case BinaryOperatorKind.Error => ???
    }
  }

  def emitBooleanLiteral(
      expr: LoweredExpression.BooleanLiteral,
      context: EmitContext
  ): unit = {
    val value = if (expr.value) 1 else 0
    emitLoadConstant(value, expr.location.startLine)
  }

  def emitCallExpression(
      expr: LoweredExpression.Call,
      context: EmitContext
  ): unit = ???

  def emitCastExpression(
      expr: LoweredExpression.Cast,
      context: EmitContext
  ): unit = ???

  def emitCharacterLiteral(
      expr: LoweredExpression.CharacterLiteral,
      context: EmitContext
  ): unit = ???

  def emitIntLiteral(
      expr: LoweredExpression.IntegerLiteral,
      context: EmitContext
  ): unit = {
    chunk.emitOpcode(Opcode.LdcI4, expr.location.startLine)
    chunk.emitI4(expr.value, expr.location.startLine)
  }

  def emitMemberAccess(
      expr: LoweredExpression.MemberAccess,
      context: EmitContext
  ): unit = ???

  def emitNewExpression(
      expr: LoweredExpression.New,
      context: EmitContext
  ): unit = ???

  def emitStringLiteral(
      expr: LoweredExpression.StringLiteral,
      context: EmitContext
  ): unit = {
    val token = metadata.addString(expr.value)
    chunk.emitOpcode(Opcode.Ldstr, expr.location.startLine)
    chunk.emitI4(token.token, expr.location.startLine)
  }

  def emitUnaryExpression(
      expr: LoweredExpression.Unary,
      context: EmitContext
  ): unit = {
    emitExpression(expr.operand, context)
    expr.operator match {
      case UnaryOperatorKind.Identity => // do nothing
      case UnaryOperatorKind.Negation => // -x
        chunk.emitOpcode(Opcode.Neg, expr.location.startLine)

      case UnaryOperatorKind.LogicalNegation => // !x
        emitLoadConstant(0, expr.location.startLine)
        chunk.emitOpcode(Opcode.Ceq, expr.location.startLine)

      case UnaryOperatorKind.BitwiseNegation => // ~x
        chunk.emitOpcode(Opcode.Not, expr.location.startLine)

      case UnaryOperatorKind.Error => ???
    }
  }

  def emitUnitExpression(
      context: EmitContext
  ): unit = {
    chunk.emitOpcode(Opcode.Nop, -1)
  }

  def emitVariable(
      expr: LoweredExpression.Variable,
      context: EmitContext
  ): unit = {
    val index = context.getLocalIndex(expr.symbol)
    if (index < 4) {
      chunk.emitOpcode(Opcode.Ldloc0 + index, expr.location.startLine)
    } else {
      chunk.emitOpcode(Opcode.Ldlocn, expr.location.startLine)
      chunk.emitI4(index, expr.location.startLine)
    }
  }

  def emitLoadConstant(i: int, startLine: int): unit = {
    chunk.emitOpcode(Opcode.LdcI4, startLine)
    chunk.emitI4(i, startLine)
  }

  def emitStatements(
      statements: Chain[LoweredStatement],
      context: EmitContext
  ): unit = {
    statements.uncons() match {
      case Option.None =>
      case Option.Some(Tuple2(statement, rest)) =>
        emitStatement(statement, context)
        emitStatements(rest, context)
    }
  }

  def emitStatement(statement: LoweredStatement, context: EmitContext): unit = {
    statement match {
      case LoweredStatement.Error => panic("emitStatement: Error")

      case value: LoweredStatement.ExpressionStatement =>
        emitExpressionStatement(value, context)
//      case value: LoweredStatement.VariableDeclaration =>
//        emitVariableDeclaration(value, context)
      case statement: LoweredStatement.Assignment =>
        emitAssignmentStatement(statement, context)
      case statement: LoweredStatement.ConditionalGoto =>
        emitConditionalGotoStatement(statement, context)
      case statement: LoweredStatement.Goto =>
        emitGotoStatement(statement, context)
      case statement: LoweredStatement.LabelDeclaration =>
        emitLabelDeclarationStatement(statement, context)
      case statement: LoweredStatement.Return =>
        emitReturnStatement(statement, context)
    }
  }

  def emitConditionalGotoStatement(
      statement: LoweredStatement.ConditionalGoto,
      context: EmitContext
  ): unit = {
    emitExpression(statement.condition, context)
    val op = if (statement.invert) Opcode.Brfalse else Opcode.Brtrue
    chunk.emitOpcode(op, statement.location.startLine)

    val target = context.getBranchTarget(statement.label)
    chunk.emitI4(target, statement.location.startLine)
  }

  def emitGotoStatement(
      statement: LoweredStatement.Goto,
      context: EmitContext
  ): unit = {
    chunk.emitOpcode(Opcode.Br, statement.location.startLine)

    val target = context.getBranchTarget(statement.label)
    chunk.emitI4(target, statement.location.startLine)
  }

  def emitLabelDeclarationStatement(
      statement: LoweredStatement.LabelDeclaration,
      context: EmitContext
  ): unit = {
    context.saveLabel(statement.label)
  }

  def emitReturnStatement(
      statement: LoweredStatement.Return,
      context: EmitContext
  ): unit = {
    ???
  }

  def emitExpressionStatement(
      statement: LoweredStatement.ExpressionStatement,
      context: EmitContext
  ): unit = {
    emitExpression(statement.expression, context)
    // TODO: emit pop? or return
  }

//  def emitVariableDeclaration(
//      statement: LoweredStatement.VariableDeclaration,
//      context: EmitContext
//  ): unit = ???

  def emitSymbolsMetadata(symbols: List[Symbol]): unit = {
    symbols match {
      case List.Nil =>
      case List.Cons(symbol, rest) =>
        emitSymbolMetadata(symbol)
        emitSymbolsMetadata(rest)
    }
  }

  def emitSymbolMetadata(symbol: Symbol): unit = {
    if (symbol.kind == SymbolKind.Namespace) {
      emitSymbolsMetadata(symbol.members())
    } else if (
      symbol.kind == SymbolKind.Class || symbol.kind == SymbolKind.Object
    ) {
      emitClassMetadata(symbol)
    } else if (symbol.kind == SymbolKind.Field) {
      emitFieldMetadata(symbol)
    } else if (
      symbol.kind == SymbolKind.Method || symbol.kind == SymbolKind.Constructor
    ) {
      emitMethodMetadata(symbol)
    } else if (symbol.kind == SymbolKind.Parameter) {
      emitParameterMetadata(symbol)
    } else if (symbol.kind == SymbolKind.Local) {
      // TODO: emit local metadata?
    } else {
      println("emitSymbolMetadata: unknown symbol kind " + symbol.kind)
      ???
    }
  }

  def ns(ns: List[string]): string = _ns(ns, "")

  def _ns(names: List[string], right: string): string = {
    names match {
      case List.Nil => right
      case List.Cons(name, rest) =>
        if (right == "") _ns(rest, name)
        else _ns(rest, name + "." + right)
    }
  }

  def emitClassMetadata(symbol: Symbol): unit = {
    val flags =
      if (symbol.kind == SymbolKind.Object)
        MetadataFlags.Static
      else MetadataFlags.None

    typeTokens = typeTokens.put(
      symbol,
      metadata.addTypeDef(symbol.name, ns(symbol.ns()), flags)
    )

    emitSymbolsMetadata(symbol.members())
  }

  def emitFieldMetadata(symbol: Symbol): unit = {
    val flags = MetadataFlags.None
//      if ((symbol.flags & SymbolFlags.Static) == SymbolFlags.Static)
//        MetadataFlags.Static
//      else MetadataFlags.None

    fieldTokens =
      fieldTokens.put(symbol, metadata.addField(symbol.name, flags, 0))

    queueSymbolSignature(symbol)
  }

  def getSymbolSignature(symbol: Symbol) = {
    binder.getSymbolType(symbol) match {
      case Option.None =>
        panic("getSymbolSignature: no type for symbol " + symbol.name)
      case Option.Some(typ) =>
        val sig = new SignatureBuilder()
        emitTypeSignature(typ, sig)
        sig.toSignature()
    }
  }

  def emitMethodMetadata(symbol: Symbol) = {
    val flags = MetadataFlags.None
//      if ((symbol.flags & SymbolFlags.Static) == SymbolFlags.Static)
//        MetadataFlags.Static
//      else MetadataFlags.None

    methodTokens =
      methodTokens.put(symbol, metadata.addMethod(symbol.name, flags, 0, 0, 0))

    queueSymbolSignature(symbol)

    emitSymbolsMetadata(symbol.members())
  }

  def emitParameterMetadata(symbol: Symbol): unit = {
    val flags = MetadataFlags.None

    paramTokens =
      paramTokens.put(symbol, metadata.addParam(symbol.name, flags, 0))

    queueSymbolSignature(symbol)
  }

  def emitTypeSignature(typ: Type, sig: SignatureBuilder): unit = {
    typ match {
      case t: Type.Class           => emitClassTypeSignature(t, sig)
      case t: Type.GenericClass    => emitGenericClassTypeSignature(t, sig)
      case t: Type.GenericFunction => emitGenericFunctionTypeSignature(t, sig)
      case t: Type.Function        => emitFunctionTypeSignature(t, sig)
      case t: Type.Variable        => emitVariableTypeSignature(t, sig)
      case Type.Any                => emitAnyTypeSignature(sig)
      case Type.Never              => emitNeverTypeSignature(sig)
      case Type.Error              => emitErrorTypeSignature(sig)
    }
  }

  def emitClassTypeSignature(typ: Type, sig: SignatureBuilder): unit = {}
  def emitGenericClassTypeSignature(typ: Type, sig: SignatureBuilder): unit = {}
  def emitGenericFunctionTypeSignature(
      typ: Type.GenericFunction,
      sig: SignatureBuilder
  ): unit = {}
  def emitFunctionTypeSignature(typ: Type, sig: SignatureBuilder): unit = {}
  def emitVariableTypeSignature(typ: Type, sig: SignatureBuilder): unit = {}
  def emitAnyTypeSignature(sig: SignatureBuilder): unit = {}
  def emitNeverTypeSignature(sig: SignatureBuilder): unit = {}
  def emitErrorTypeSignature(sig: SignatureBuilder): unit = {}
//
//  def emitTypeArraySignature(parameters: Array[Type], sig: SignatureBuilder) = {
//    sig.writeTypeArray(parameters.length)
//    for (i <- 0 to (parameters.length - 1)) {
//      emitTypeSignature(parameters(i), sig)
//    }
//  }

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

  def i4Instruction(chunk: Chunk, offset: int, name: string): int = {
    val value = chunk.readI4(offset + 1)
    println(name + " " + string(value))
    offset + 2
  }

  def simpleInstruction(name: string, offset: int): int = {
    println(name)
    offset + 1
  }
}
