import panther._

case class Diagnostic(location: TextLocation, message: string) {
  override def toString(): string = location.toString() + ": " + message
  def compareTo(other: Diagnostic): int = {
    val i = location.compareTo(other.location)
    if (i != 0) i
    else message.compareTo(other.message)
  }
}

/** A sorted list of compiler errors. Sorting is handled by the DiagnosticBag
  */
enum Diagnostics {
  case Empty
  case Node(left: Diagnostics, head: Diagnostic, tail: Diagnostics)

  def count(): int = _count(List.Cons(this, List.Nil), 0)

  def _count(items: List[Diagnostics], count: int): int =
    items match {
      case List.Nil => count
      case List.Cons(head, tail) =>
        head match {
          case Diagnostics.Empty => _count(tail, count)
          case Diagnostics.Node(left, _, right) =>
            _count(List.Cons(left, List.Cons(right, tail)), count + 1)
        }
    }

  def printDiagnostics(maxToPrint: int): int = _printDiagnostics(maxToPrint, 0)

  def _printDiagnostics(maxToPrint: int, count: int): int =
    this match {
      case Diagnostics.Empty => count
      case Diagnostics.Node(left, head, right) =>
        val leftCount = left._printDiagnostics(maxToPrint, count)
        if (leftCount <= maxToPrint) {
          printDiagnostic(head)
        } else if (leftCount == maxToPrint + 1) {
          // exceeded limit, print notice
          print("... skipping remaining diagnostics ... ")
        }
        right._printDiagnostics(maxToPrint, leftCount + 1)
    }

  def printDiagnostic(diagnostic: Diagnostic): unit = {
    val location = diagnostic.location
    val span = location.span
    val sourceFile = location.sourceFile

    println(diagnostic.toString())

    for (currentLine <- location.startLine to location.endLine) {
      val line = sourceFile.getLine(currentLine)
      val startInCurrent = sourceFile.getLineIndex(span.start) == currentLine
      val endInCurrent = sourceFile.getLineIndex(span.end) == currentLine

      val prefixEnd =
        if (startInCurrent) span.start
        else line.start

      val suffixStart =
        if (endInCurrent) span.end
        else line.end

      val prefixSpan = TextSpanFactory.fromBounds(line.start, prefixEnd)
      val errorSpan = TextSpanFactory.fromBounds(prefixEnd, suffixStart)
      val suffixSpan = TextSpanFactory.fromBounds(suffixStart, line.end + 1)

      val prefix = sourceFile.substringFromSpan(prefixSpan)
      val error = sourceFile.substringFromSpan(errorSpan)
      val suffix = sourceFile.substringFromSpan(suffixSpan)

      print(prefix)
      print(ANSI.foregroundColor("e06c75"))
      print(error)
      print(ANSI.Clear)
      println(suffix)

      for (c <- 1 to (prefixSpan.length - 1)) {
        print('-')
      }
      println('^')
    }

    println("")
  }
}

case class DiagnosticBag(settings: CompilerSettings) {
  var count: int = 0
  var diagnostics: Diagnostics = Diagnostics.Empty

  def report(location: TextLocation, message: string): unit = add(
    new Diagnostic(location, message)
  )

  def reportInternalError(location: TextLocation, extra: string): unit =
    report(location, "internal compiler error - " + extra)

  def reportNotCallable(location: TextLocation): unit =
    report(location, "expression is not callable")

  def reportTypeMismatch(
      location: TextLocation,
      expected: Type,
      actual: Type
  ): unit =
    report(location, "Expected " + expected + " but got " + actual)

  def reportTypeCircularity(location: TextLocation, name: Type): unit =
    report(location, "Circular use of variable in type" + name)

  def reportExpressionIsNotAssignable(location: TextLocation): unit =
    report(location, "expression is not assignable")

  def reportAssignmentToVal(location: TextLocation, name: string): unit =
    report(location, "reassignment to val " + name)

  def reportArgumentCountMismatch(
      location: TextLocation,
      expected: int,
      actual: int
  ): unit =
    report(location, "Expected " + expected + " arguments, but got " + actual)

  def reportCannotConvert(
      location: TextLocation,
      from: Type,
      toType: Type
  ): unit =
    report(location, "Cannot convert from " + from + " to " + toType)

  def reportSymbolNotFoundForType(
      location: TextLocation,
      left: Type,
      name: string
  ): unit =
    report(location, "Symbol " + name + " not found for type " + left)

  def reportSymbolNotFound(location: TextLocation, name: string): unit =
    report(location, "Symbol " + name + " not found")

  def reportTypeNotDefined(location: TextLocation, name: string): unit =
    report(location, "Type " + name + " not defined")

  /** A trait declares a capability, so there is nothing to construct. Without
    * this the `new` path falls through to "not found", which is misleading
    * because the name did resolve.
    */
  def reportTraitNotInstantiable(location: TextLocation, name: string): unit =
    report(location, "Trait " + name + " cannot be instantiated")

  /** A call to a constrained generic whose constraint nothing proves. */
  def reportNoGivenInstance(location: TextLocation, goal: string): unit =
    report(location, "No given instance for " + goal)

  /** Calling a constrained generic from inside a generic that declares no
    * matching bound. There is no given to find — the type is not known here —
    * and nothing to forward either.
    */
  def reportUnconstrainedTypeParameter(
      location: TextLocation,
      goal: string
  ): unit =
    report(
      location,
      "No evidence for " + goal +
        "; the enclosing declaration does not require it"
    )

  /** A chain of conditional givens that does not get smaller. */
  def reportEvidenceTooDeep(location: TextLocation, goal: string): unit =
    report(
      location,
      "Evidence for " + goal + " is too deeply nested; a given's premise may " +
        "not be larger than what it proves"
    )

  /** `given Foo[int]` where `Foo` is a class. A given proves a trait holds. */
  def reportGivenHeadNotATrait(location: TextLocation, name: string): unit =
    report(location, name + " is not a trait, so it cannot have a given")

  /** `given Eq { … }` — the trait is not applied to anything, so the
    * declaration does not say which type it is evidence for.
    */
  def reportGivenHeadMissingArguments(
      location: TextLocation,
      name: string
  ): unit =
    report(location, "Given for " + name + " needs type arguments")

  /** Global coherence: at most one given per `(trait, type)` pair in the whole
    * program. Overlap is unification, so `Ord[List[T]]` collides with
    * `Ord[List[int]]`.
    */
  def reportOverlappingGiven(
      location: TextLocation,
      head: string,
      existing: TextLocation
  ): unit =
    report(
      location,
      "Given for " + head + " overlaps the one at " + existing.toString()
    )

  /** `[K: Foo]` where `Foo` is a class or object. A context bound asks for
    * evidence, and only a trait can be evidenced.
    */
  def reportContextBoundNotATrait(location: TextLocation, name: string): unit =
    report(location, name + " is not a trait and cannot be a context bound")

  /** `[K: Eq]` means `Eq[K]`, so the trait has to have exactly one type
    * parameter for `K` to fill.
    */
  def reportContextBoundArity(
      location: TextLocation,
      name: string,
      found: int
  ): unit =
    report(
      location,
      "Trait " + name + " takes " + string(found) +
        " type parameters; a context bound requires exactly 1"
    )

  /** `operator ~(…)`. Only a token the language already parses as a binary
    * operator may be declared — new syntax comes from a language change, not
    * from a library
    * ([ADR 0004](../../../docs/architecture/adr/0004-traits-given-evidence-and-contextual-extensions.md)).
    */
  /** `[trace(…)]`. `derive` is the only attribute the language has. */
  def reportUnknownAttribute(location: TextLocation, name: string): unit =
    report(location, name + " is not an attribute")

  /** `[derive(Eq)] def f() = 1`. Derivation is over constructor parameters, so
    * only a class or an enum has anything to derive
    * ([ADR 0004](../../../docs/architecture/adr/0004-traits-given-evidence-and-contextual-extensions.md)).
    */
  def reportDeriveNotSupported(location: TextLocation, what: string): unit =
    report(location, "derive cannot be applied to " + what)

  /** `[derive(Printable)]`. Derivation is a rule about what a trait means over
    * a list of parameters, and there is no such rule for a trait the compiler
    * has never seen.
    */
  def reportTraitNotDerivable(location: TextLocation, name: string): unit =
    report(
      location,
      name + " cannot be derived; only Eq, Ord and Show can"
    )

  /** A generic type's derived given is conditional — `Eq[Box[T]]` given
    * `Eq[T]` — and a conditional given cannot reach its own premise yet
    * ([ADR 0004](../../../docs/architecture/adr/0004-traits-given-evidence-and-contextual-extensions.md)).
    */
  def reportDeriveOnGenericType(location: TextLocation, name: string): unit =
    report(location, "Cannot derive for " + name + ": it has type parameters")

  /** An enum's derived members have to match the cases first and only then
    * their parameters, which is not what the rule over constructor parameters
    * describes.
    */
  def reportDeriveOnEnum(location: TextLocation): unit =
    report(location, "Cannot derive for an enum yet")

  /** Derivation needs evidence for every parameter type, and says which one is
    * missing rather than reporting the type as a whole.
    */
  def reportNoEvidenceForDerivedField(
      location: TextLocation,
      field: string,
      traitName: string
  ): unit =
    report(
      location,
      "Cannot derive " + traitName + ": no " + traitName +
        " evidence for the type of " + field
    )

  def reportNotABinaryOperator(location: TextLocation, op: string): unit =
    report(location, op + " is not a binary operator and cannot be declared")

  /** A token belongs to one trait. Without that rule `a == b` would need
    * overload resolution across traits, and the point of the design is that a
    * use site resolves to one piece of evidence
    * ([ADR 0004](../../../docs/architecture/adr/0004-traits-given-evidence-and-contextual-extensions.md)).
    */
  def reportOperatorAlreadyClaimed(
      location: TextLocation,
      op: string,
      owner: string
  ): unit =
    report(location, "Operator " + op + " is already declared by " + owner)

  def reportInvalidOperator(location: TextLocation, op: string): unit =
    report(location, "Invalid operator: " + op)

  def reportBugUnknownType(location: TextLocation, name: string): unit =
    report(location, "Bug: type could not be determined for variable " + name)

  def reportNoOperatorForOperands(
      location: TextLocation,
      op: string,
      left: Type,
      right: Type
  ): unit =
    report(
      location,
      "No operator '" + op + "' for operands " + left + " and " + right
    )

  def reportNoOperatorForOperand(
      location: TextLocation,
      op: string,
      operand: Type
  ): unit =
    report(location, "No operator '" + op + "' for operand " + operand)

  def reportTopLevelStatementsInMultipleFiles(
      firstLocation: TextLocation,
      secondLocation: TextLocation
  ): unit =
    report(
      firstLocation,
      "Top-level statements in multiple files " + firstLocation
        .toString() + " and " + secondLocation.toString()
    )

  def reportDuplicateDefinition(
      name: string,
      first: TextLocation,
      second: TextLocation
  ): unit =
    report(
      first,
      "Duplicate definition of " + name + " at " + second.toString()
    )

  def reportMultipleEntryPoints(
      first: TextLocation,
      second: TextLocation
  ): unit =
    report(
      first,
      "Multiple entry points in files " + first.toString() + " and " + second
        .toString()
    )

  def reportInvalidNamespace(location: TextLocation): unit =
    report(location, "Invalid namespace")

  def reportUnsupportedStatement(
      location: TextLocation,
      keyword: string
  ): unit =
    report(location, keyword + " is not supported")

  def reportBadCharacter(location: TextLocation, value: char): unit =
    report(location, "Invalid character in input: " + string(value))

  def reportEmptyCharLiteral(location: TextLocation): unit =
    report(location, "Empty character literal")

  def reportUnterminatedBlockComment(location: TextLocation): unit =
    report(location, "Unterminated block comment")

  def reportUnterminatedChar(location: TextLocation): unit =
    report(location, "Unterminated character literal")

  def reportUnterminatedString(location: TextLocation): unit =
    report(location, "Unterminated string literal")

  def reportExpectedExpression(location: TextLocation, currentKind: int): unit =
    report(
      location,
      "Unexpected token " + SyntaxFacts.getKindName(
        currentKind
      ) + ", expected expression"
    )

  def reportUnexpectedToken(
      location: TextLocation,
      currentKind: int,
      expectedKind: int
  ): unit =
    report(
      location,
      "Unexpected token " + SyntaxFacts.getKindName(
        currentKind
      ) + ", expected " + SyntaxFacts.getKindName(expectedKind)
    )

  def reportInvalidEscapeSequence(location: TextLocation, current: char): unit =
    report(location, "Invalid character in escape sequence: " + string(current))

  def reportExpectedPattern(location: TextLocation, currentKind: int): unit =
    report(
      location,
      "Unexpected token " + SyntaxFacts.getKindName(
        currentKind
      ) + ", expected pattern"
    )

  def reportInvalidPattern(location: TextLocation): unit =
    report(location, "Invalid pattern")

  def add(diagnostic: Diagnostic): unit = {
    count = count + 1
    diagnostics = _insert(diagnostics, diagnostic)
  }

  def _insert(node: Diagnostics, diagnostic: Diagnostic): Diagnostics = {
    node match {
      case Diagnostics.Empty =>
        Diagnostics.Node(Diagnostics.Empty, diagnostic, Diagnostics.Empty)
      case Diagnostics.Node(left, head, tail) =>
        val cmp = diagnostic.compareTo(head)
        if (cmp == 0) {
          node
        } else if (diagnostic.compareTo(head) < 0) {
          Diagnostics.Node(_insert(left, diagnostic), head, tail)
        } else {
          Diagnostics.Node(left, head, _insert(tail, diagnostic))
        }
    }
  }

  def addDiagnostics(more: Diagnostics): unit = more match {
    case Diagnostics.Empty => ()
    case Diagnostics.Node(left, head, right) =>
      add(head)
      addDiagnostics(left)
      addDiagnostics(right)
  }

  def printDiagnostics(): int = {
    diagnostics.printDiagnostics(settings.diagnosticsToPrint)
  }
}
