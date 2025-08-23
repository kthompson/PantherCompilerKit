import panther.*
import system.io.*

case class EmitResult(
    chunk: Chunk,
    metadata: Metadata,
    entry: Option[MethodToken]
)

case class EmitContext(
    chunk: Chunk,
    method: MethodMetadata,
    // map of local symbols to their method local index
    var locals: Dictionary[Symbol, int],
    var params: Dictionary[Symbol, int]
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

  def getParamIndex(symbol: Symbol): int = {
    params.get(symbol) match {
      case Option.Some(value) => value
      case Option.None =>
        panic("getParamIndex: no parameter index for " + symbol.name)
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
  var globalFieldIndex = 0
  var classFieldIndex = 0

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
    emitSymbolMetadata(root, true)

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
          DictionaryModule.empty[Symbol, int](),
          getMethodParameterMap(
            0,
            key.members(),
            DictionaryModule.empty[Symbol, int]()
          )
        )
        emitMethod(key, context)
        context.applyPatches()

        emitMethodBodies(Dictionary(tail))
    }
  }

  def getMethodParameterMap(
      index: int,
      symbols: List[Symbol],
      map: Dictionary[Symbol, int]
  ): Dictionary[Symbol, int] = {
    symbols match {
      case List.Nil => map
      case List.Cons(head, tail) =>
        if (head.kind == SymbolKind.Parameter) {
          val newMap = map.put(head, index)
          getMethodParameterMap(index + 1, tail, newMap)
        } else {
          getMethodParameterMap(index, tail, map)
        }
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
      case value: LoweredExpression.Boolean =>
        emitBooleanLiteral(value, context)
      case value: LoweredExpression.Call =>
        emitCallExpression(value, context)
      case value: LoweredExpression.Cast =>
        emitCastExpression(value, context)
      case value: LoweredExpression.Character =>
        emitCharacterLiteral(value, context)
      case value: LoweredExpression.Integer =>
        emitIntLiteral(value, context)
      case value: LoweredExpression.MemberAccess =>
        emitMemberAccess(value, context)
      case value: LoweredExpression.New =>
        emitNewExpression(value, context)
      case value: LoweredExpression.String =>
        emitStringLiteral(value, context)
      case value: LoweredExpression.This =>
        emitThisExpression(value, context)
      case value: LoweredExpression.Unary =>
        emitUnaryExpression(value, context)
      case value: LoweredExpression.Variable =>
        emitVariable(value, context)
      case value: LoweredExpression.TypeCheck =>
        emitTypeCheckExpression(value, context)
      case LoweredExpression.Unit =>
        emitUnitExpression(context)
    }
  }

  def emitAssignFieldStatement(
      expr: LoweredStatement.AssignField,
      context: EmitContext
  ): unit = {
    emitLHS(expr.receiver, context)
    emitExpression(expr.expression, context)
    assert(expr.field.kind == SymbolKind.Field, "expected field")
    fieldTokens.get(expr.field) match {
      case Option.Some(value) =>
        assert(!expr.field.isStatic(), "expected non-static field")
        chunk.emitOpcode(Opcode.Stfld, expr.location.startLine)
        chunk.emitI4(value.token, expr.location.startLine)
      case Option.None =>
        panic("emitAssignmentStatement: no field token for " + expr.field)
    }
  }

  def emitAssignStaticField(
      expr: LoweredStatement.AssignStaticField,
      context: EmitContext
  ): unit = {
    assert(expr.field.kind == SymbolKind.Field, "expected field")
    assert(expr.field.isStatic(), "expected static field")

    fieldTokens.get(expr.field) match {
      case Option.Some(value) =>
        emitExpression(expr.expression, context)
        chunk.emitOpcode(Opcode.Stsfld, expr.location.startLine)
        chunk.emitI4(value.token, expr.location.startLine)
      case Option.None =>
        panic("emitAssignStaticField: no field token for " + expr.field)
    }
  }

  def emitAssignLocalStatement(
      expr: LoweredStatement.AssignLocal,
      context: EmitContext
  ): unit = {
    emitExpression(expr.expression, context)
    assert(expr.local.kind == SymbolKind.Local, "expected local variable")
    val index = context.getLocalIndex(expr.local)
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

        emitLoadBool(false, expr.location.startLine)
        chunk.emitOpcode(Opcode.Ceq, expr.location.startLine)

      case BinaryOperatorKind.LessThan =>
        chunk.emitOpcode(Opcode.Clt, expr.location.startLine)

      case BinaryOperatorKind.LessThanOrEqual =>
        chunk.emitOpcode(Opcode.Cgt, expr.location.startLine)

        emitLoadBool(false, expr.location.startLine)
        chunk.emitOpcode(Opcode.Ceq, expr.location.startLine)

      case BinaryOperatorKind.GreaterThan =>
        chunk.emitOpcode(Opcode.Cgt, expr.location.startLine)

      case BinaryOperatorKind.GreaterThanOrEqual =>
        chunk.emitOpcode(Opcode.Clt, expr.location.startLine)

        emitLoadBool(false, expr.location.startLine)
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
      expr: LoweredExpression.Boolean,
      context: EmitContext
  ): unit =
    emitLoadBool(expr.value, expr.location.startLine)

  def emitCallExpression(
      expr: LoweredExpression.Call,
      context: EmitContext
  ): unit = {
    // emit receiver
    expr.receiver match {
      case Option.None => // no receiver, static call
      case Option.Some(receiver) =>
        emitLHS(receiver, context)
    }
    // emit args
    emitExpressions(expr.arguments, context)

    // emit call opcode
    val startLine = expr.location.startLine
    methodTokens.get(expr.method) match {
      case Option.None =>
        panic("emitCallExpression: no method token for " + expr.method.name)
      case Option.Some(token) =>
        chunk.emitOpcode(Opcode.Call, startLine)
        chunk.emitI4(token.token, startLine)
    }
  }

  def emitExpressions(
      value: Chain[LoweredExpression],
      context: EmitContext
  ): unit = {
    value.uncons() match {
      case Option.None => ()
      case Option.Some(Tuple2(expr, rest)) =>
        emitExpression(expr, context)
        emitExpressions(rest, context)
    }
  }

  def emitLHS(
      lhs: LoweredLeftHandSide,
      context: EmitContext
  ): unit = {
    lhs match {
      case LoweredLeftHandSide.MemberAccess(_, left, symbol) =>
        // Emit the receiver for instance member access
        emitLHS(left, context)
      case LoweredLeftHandSide.Variable(location, variable) =>
        emitVariable(LoweredExpression.Variable(location, variable), context)
      case LoweredLeftHandSide.New(
            location,
            constructor,
            genericArguments,
            arguments,
            resultType
          ) =>
        // Emit the new expression
        emitNewExpression(
          LoweredExpression.New(
            location,
            constructor,
            genericArguments,
            arguments,
            resultType
          ),
          context
        )
    }
  }

  def emitCastExpression(
      expr: LoweredExpression.Cast,
      context: EmitContext
  ): unit = ???

  def emitCharacterLiteral(
      expr: LoweredExpression.Character,
      context: EmitContext
  ): unit = ???

  def emitIntLiteral(
      expr: LoweredExpression.Integer,
      context: EmitContext
  ): unit = {
    chunk.emitOpcode(Opcode.LdcI4, expr.location.startLine)
    chunk.emitI4(expr.value, expr.location.startLine)
  }

  def emitMemberAccess(
      expr: LoweredExpression.MemberAccess,
      context: EmitContext
  ): unit = {
    if (expr.symbol.kind == SymbolKind.Method) {
      methodTokens.get(expr.symbol) match {
        case Option.Some(token) =>
          if (!expr.symbol.isStatic()) {
            // Emit the receiver for instance methods
            emitLHS(expr.left, context)
          }
          chunk.emitOpcode(Opcode.Call, expr.location.startLine)
          chunk.emitI4(token.token, expr.location.startLine)
        case Option.None =>
          panic("emitMemberAccess: no method token for " + expr.symbol)
      }
    } else if (expr.symbol.kind == SymbolKind.Field) {
      fieldTokens.get(expr.symbol) match {
        case Option.Some(value) =>
          if (expr.symbol.isStatic()) {
            // Static field access
            chunk.emitOpcode(Opcode.Ldsfld, expr.location.startLine)
          } else {
            // Instance field access - emit the receiver first
            emitLHS(expr.left, context)
            chunk.emitOpcode(Opcode.Ldfld, expr.location.startLine)
          }
          chunk.emitI4(value.token, expr.location.startLine)
        case Option.None =>
          panic(
            "emitMemberAccess: no field token for " + expr.symbol
              .qualifiedName()
          )
      }
    } else {
      panic("emitMemberAccess: unsupported symbol kind " + expr.symbol.kind)
    }
  }

  def emitNewExpression(
      expr: LoweredExpression.New,
      context: EmitContext
  ): unit = {
    // Emit the constructor arguments on the stack first
    emitExpressions(expr.arguments, context)

    // Get the constructor token and emit the newobj opcode
    methodTokens.get(expr.constructor) match {
      case Option.None =>
        panic(
          "emitNewExpression: no method token for " + expr.constructor.kind + " '" + expr.constructor.name + "'"
        )
      case Option.Some(token) =>
        chunk.emitOpcode(Opcode.Newobj, expr.location.startLine)
        chunk.emitI4(token.token, expr.location.startLine)
    }
  }

  def emitStringLiteral(
      expr: LoweredExpression.String,
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

  def emitThisExpression(
      expr: LoweredExpression.This,
      context: EmitContext
  ): unit = {
    // Load the 'this' parameter, which is typically the first parameter (index 0)
    chunk.emitOpcode(Opcode.Ldarg0, expr.location.startLine)
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
    if (expr.symbol.kind == SymbolKind.Parameter) {
      emitParameter(expr, context)
    } else if (expr.symbol.kind == SymbolKind.Local) {
      emitLocal(expr, context)
    } else if (expr.symbol.kind == SymbolKind.Field) {
      emitField(expr, context)
    } else {
      panic("emitVariable: unsupported symbol kind " + expr.symbol.kind)
    }
  }

  def emitField(
      expr: LoweredExpression.Variable,
      context: EmitContext
  ): unit = {
    fieldTokens.get(expr.symbol) match {
      case Option.None =>
        panic("emitField: no field token for " + expr.symbol.name)
      case Option.Some(token) =>
        val op = if (expr.symbol.isStatic()) Opcode.Ldsfld else Opcode.Ldfld
        chunk.emitOpcode(op, expr.location.startLine)
        chunk.emitI4(token.token, expr.location.startLine)
    }
  }

  def emitParameter(
      expr: LoweredExpression.Variable,
      context: EmitContext
  ): unit = {
    val index = context.getParamIndex(expr.symbol)
    if (index < 4) {
      chunk.emitOpcode(Opcode.Ldarg0 + index, expr.location.startLine)
    } else {
      chunk.emitOpcode(Opcode.Ldargn, expr.location.startLine)
      chunk.emitI4(index, expr.location.startLine)
    }
  }

  def emitLocal(
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

  def emitLoadBool(b: bool, startLine: int): unit = {
    val op = if (b) Opcode.Ldtrue else Opcode.Ldfalse
    chunk.emitOpcode(op, startLine)
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
      case statement: LoweredStatement.AssignLocal =>
        emitAssignLocalStatement(statement, context)
      case statement: LoweredStatement.AssignField =>
        emitAssignFieldStatement(statement, context)
      case statement: LoweredStatement.AssignStaticField =>
        emitAssignStaticField(statement, context)
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

  def emitSymbolsMetadata(symbols: List[Symbol], isStatic: bool): unit = {
    symbols match {
      case List.Nil =>
      case List.Cons(symbol, rest) =>
        emitSymbolMetadata(symbol, isStatic)
        emitSymbolsMetadata(rest, isStatic)
    }
  }

  def emitSymbolMetadata(symbol: Symbol, parentStatic: bool): unit = {
    symbol.kind match {
      case SymbolKind.Namespace   => emitSymbolsMetadata(symbol.members(), true)
      case SymbolKind.Object      => emitClassMetadata(symbol)
      case SymbolKind.Class       => emitClassMetadata(symbol)
      case SymbolKind.Alias       => emitClassMetadata(symbol)
      case SymbolKind.Field       => emitFieldMetadata(symbol, parentStatic)
      case SymbolKind.Method      => emitMethodMetadata(symbol, parentStatic)
      case SymbolKind.Constructor => emitMethodMetadata(symbol, parentStatic)
      case SymbolKind.Parameter   => emitParameterMetadata(symbol)

      case _: SymbolKind.TypeParameter => // TODO: emit type parameter metadata??
      case SymbolKind.Local => // TODO: emit local metadata?
      case SymbolKind.Block =>
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
    val isObject = symbol.kind == SymbolKind.Object
    val flags =
      if (isObject) MetadataFlags.Static
      else MetadataFlags.None

    typeTokens = typeTokens.put(
      symbol,
      metadata.addTypeDef(symbol.name, ns(symbol.ns()), flags)
    )

    emitSymbolsMetadata(symbol.members(), isObject)
  }

  def emitFieldMetadata(symbol: Symbol, isStatic: bool): unit = {
    val flags =
      if (isStatic) MetadataFlags.Static
      else MetadataFlags.None

    val index = if (isStatic) {
      globalFieldIndex
    } else {
      classFieldIndex
    }

    if (isStatic) {
      globalFieldIndex = globalFieldIndex + 1
    } else {
      classFieldIndex = classFieldIndex + 1
    }

    fieldTokens = fieldTokens.put(
      symbol,
      metadata.addField(symbol.name, flags, index, 0)
    )

    queueSymbolSignature(symbol)
  }

  def getSymbolSignature(symbol: Symbol) = {
    binder.tryGetSymbolType(symbol) match {
      case Option.None =>
        panic("getSymbolSignature: no type for symbol " + symbol.fullName())
      case Option.Some(typ) =>
        val sig = new SignatureBuilder()
        emitTypeSignature(typ, sig)
        sig.toSignature()
    }
  }

  def emitMethodMetadata(symbol: Symbol, isStatic: bool) = {
    val flags =
      if (isStatic) MetadataFlags.Static
      else MetadataFlags.None

    methodTokens = methodTokens.put(
      symbol,
      metadata.addMethod(symbol.name, flags, !isStatic, 0, 0, -1)
    )

    queueSymbolSignature(symbol)

    emitSymbolsMetadata(symbol.members(), isStatic)
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
      case t: Type.Alias           => emitAliasTypeSignature(t, sig)
      case t: Type.Union           => emitUnionTypeSignature(t, sig)
      case t: Type.GenericClass    => emitGenericClassTypeSignature(t, sig)
      case t: Type.GenericFunction => emitGenericFunctionTypeSignature(t, sig)
      case t: Type.Function        => emitFunctionTypeSignature(t, sig)
      case t: Type.Variable        => emitVariableTypeSignature(t, sig)
      case Type.Any                => emitAnyTypeSignature(sig)
      case Type.Never              => emitNeverTypeSignature(sig)
      case Type.Error(_)           => emitErrorTypeSignature(sig)
    }
  }

  def getTypeDefToken(typ: Type): Option[TypeDefToken] = {
    typ match {
      case Type.Class(_, _, _, _, symbol) =>
        typeTokens.get(symbol)
      case Type.GenericClass(_, _, _, _, symbol) =>
        typeTokens.get(symbol)
      case Type.Alias(_, _, _, _, _, symbol) =>
        typeTokens.get(symbol)
      case _ =>
        Option.None
    }
  }

  def emitTypeCheckExpression(
      expr: LoweredExpression.TypeCheck,
      context: EmitContext
  ): unit = {
    // Emit the inner expression to be checked
    emitExpression(expr.expression, context)

    // Get the TypeDefToken for the expected type
    getTypeDefToken(expr.expectedType) match {
      case Option.None =>
        panic(
          "emitTypeCheckExpression: no TypeDefToken for type " + expr.expectedType.toString
        )
      case Option.Some(typeToken) =>
        // Emit IsInstance opcode with the type token
        chunk.emitOpcode(Opcode.IsInst, expr.location.startLine)
        chunk.emitI4(typeToken.token, expr.location.startLine)
    }
  }

  def emitAliasTypeSignature(typ: Type, sig: SignatureBuilder): unit = {}
  def emitUnionTypeSignature(typ: Type, sig: SignatureBuilder): unit = {}

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

}
