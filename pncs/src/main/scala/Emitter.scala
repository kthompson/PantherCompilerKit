import panther._
import system.io._

case class EmitResult(
    chunk: Chunk,
    metadata: Metadata,
    entry: Option[MethodToken]
)

case class EmitContext(
    startAddress: int,
    method: MethodMetadata,
    var locals: Dictionary[Symbol, int]
) {
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

case class Emitter(
    syntaxTrees: List[SyntaxTree],
    root: Symbol,
    binder: Binder,
    assembly: BoundAssembly
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
          chunk.size,
          metadata.methods.methods(value.token),
          DictionaryModule.empty[Symbol, int]()
        )
        emitMethod(key, context)

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
        emitExpression(value, context)
        chunk.emitOpcode(Opcode.Ret, symbol.location.endLine)
        context.method.address = context.startAddress
        true
    }
  }

  def emitExpression(expr: BoundExpression, context: EmitContext): unit = {
    expr match {
      case BoundExpression.Error => panic("emitExpression: Error")
      case value: BoundExpression.Assignment =>
        emitAssignment(value, context)
      case value: BoundExpression.BinaryExpression =>
        emitBinaryExpression(value, context)
      case value: BoundExpression.Block => emitBlock(value, context)
      case value: BoundExpression.BooleanLiteral =>
        emitBooleanLiteral(value, context)
      case value: BoundExpression.CallExpression =>
        emitCallExpression(value, context)
      case value: BoundExpression.CastExpression =>
        emitCastExpression(value, context)
      case value: BoundExpression.CharacterLiteral =>
        emitCharacterLiteral(value, context)
      case value: BoundExpression.ForExpression =>
        emitForExpression(value, context)
      case value: BoundExpression.IfExpression =>
        emitIfExpression(value, context)
      case value: BoundExpression.IndexExpression =>
        emitIndexExpression(value, context)
      case value: BoundExpression.IntLiteral =>
        emitIntLiteral(value, context)
      case value: BoundExpression.MemberAccess =>
        emitMemberAccess(value, context)
      case value: BoundExpression.NewExpression =>
        emitNewExpression(value, context)
      case value: BoundExpression.StringLiteral =>
        emitStringLiteral(value, context)
      case value: BoundExpression.UnaryExpression =>
        emitUnaryExpression(value, context)
      case value: BoundExpression.UnitExpression =>
        emitUnitExpression(value, context)
      case value: BoundExpression.Variable =>
        emitVariable(value, context)
      case value: BoundExpression.WhileExpression =>
        emitWhileExpression(value, context)
    }
  }

  def emitAssignment(
      expr: BoundExpression.Assignment,
      context: EmitContext
  ): unit = {
    emitExpression(expr.expression, context)
    context.getLocalIndex(expr.variable) match {
      case index if index < 4 =>
        chunk.emitOpcode(Opcode.Stloc0 + index, expr.location.startLine)
      case index =>
        chunk.emitOpcode(Opcode.Stlocn, expr.location.startLine)
        chunk.emitI4(index, expr.location.startLine)
    }
  }

  def emitBinaryExpression(
      expr: BoundExpression.BinaryExpression,
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

  def emitBlock(expr: BoundExpression.Block, context: EmitContext): unit = {
    emitStatements(expr.statements, context)
    emitExpression(expr.expression, context)
  }

  def emitBooleanLiteral(
      expr: BoundExpression.BooleanLiteral,
      context: EmitContext
  ): unit = {
    val value = if (expr.value) 1 else 0
    emitLoadConstant(value, expr.location.startLine)
  }

  def emitCallExpression(
      expr: BoundExpression.CallExpression,
      context: EmitContext
  ): unit = ???

  def emitCastExpression(
      expr: BoundExpression.CastExpression,
      context: EmitContext
  ): unit = ???

  def emitCharacterLiteral(
      expr: BoundExpression.CharacterLiteral,
      context: EmitContext
  ): unit = ???

  def emitForExpression(
      expr: BoundExpression.ForExpression,
      context: EmitContext
  ): unit = ???

  def emitIfExpression(
      expr: BoundExpression.IfExpression,
      context: EmitContext
  ): unit = ???

  def emitIndexExpression(
      expr: BoundExpression.IndexExpression,
      context: EmitContext
  ): unit = ???

  def emitIntLiteral(
      expr: BoundExpression.IntLiteral,
      context: EmitContext
  ): unit = {
    chunk.emitOpcode(Opcode.LdcI4, expr.location.startLine)
    chunk.emitI4(expr.value, expr.location.startLine)
  }

  def emitMemberAccess(
      expr: BoundExpression.MemberAccess,
      context: EmitContext
  ): unit = ???

  def emitNewExpression(
      expr: BoundExpression.NewExpression,
      context: EmitContext
  ): unit = ???

  def emitStringLiteral(
      expr: BoundExpression.StringLiteral,
      context: EmitContext
  ): unit = {
    val token = metadata.addString(expr.value)
    chunk.emitOpcode(Opcode.Ldstr, expr.location.startLine)
    chunk.emitI4(token.token, expr.location.startLine)
  }

  def emitUnaryExpression(
      expr: BoundExpression.UnaryExpression,
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
      expr: BoundExpression.UnitExpression,
      context: EmitContext
  ): unit = {
    chunk.emitOpcode(Opcode.Nop, expr.location.startLine)
  }

  def emitVariable(expr: BoundExpression.Variable, context: EmitContext): unit = {
    context.getLocalIndex(expr.symbol) match {
      case index if index < 4 =>
        chunk.emitOpcode(Opcode.Ldloc0 + index, expr.location.startLine)
      case index =>
        chunk.emitOpcode(Opcode.Ldlocn, expr.location.startLine)
        chunk.emitI4(index, expr.location.startLine)
    }
  }

  def emitWhileExpression(
      expr: BoundExpression.WhileExpression,
      context: EmitContext
  ): unit = ???

  def emitLoadConstant(i: int, startLine: int): unit = {
    chunk.emitOpcode(Opcode.LdcI4, startLine)
    chunk.emitI4(i, startLine)
  }

  def emitStatements(
      statements: List[BoundStatement],
      context: EmitContext
  ): unit = {
    statements match {
      case List.Nil =>
      case List.Cons(statement, rest) =>
        emitStatement(statement, context)
        emitStatements(rest, context)
    }
  }

  def emitStatement(statement: BoundStatement, context: EmitContext): unit = {
    statement match {
      case BoundStatement.Error => panic("emitStatement: Error")

      case value: BoundStatement.ExpressionStatement =>
        emitExpressionStatement(value, context)
      case value: BoundStatement.VariableDeclaration =>
        emitVariableDeclaration(value, context)
    }
  }

  def emitExpressionStatement(
      statement: BoundStatement.ExpressionStatement,
      context: EmitContext
  ): unit = {
    emitExpression(statement.expression, context)
    // TODO: emit pop? or return
  }

  def emitVariableDeclaration(
      statement: BoundStatement.VariableDeclaration,
      context: EmitContext
  ): unit = ???

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
