import panther._

import MemberSyntax._
import SimpleNameSyntax._

/**
    separatorToken may be either a `.` or `,` in the case
*/

case class TypeAnnotationSyntax(colonToken: SyntaxToken, typ: NameSyntax)

case class ArrayInitializerExpressionSyntax(
    openBrace: SyntaxToken,
    expressions: ExpressionListSyntax,
    closeBrace: SyntaxToken
)

enum SimpleNameSyntax {
    case GenericNameSyntax(identifier: SyntaxToken,
                           typeArgumentlist: TypeArgumentListSyntax)
    case IdentifierNameSyntax(identifier: SyntaxToken)

    case ScalaAliasSyntax(open: SyntaxToken, name: SyntaxToken, arrow: SyntaxToken, alias: SyntaxToken, close: SyntaxToken)
    case AliasSyntax(name: SyntaxToken, asKeyword: SyntaxToken, alias: SyntaxToken)
}

enum NameSyntax {
    case SimpleName(name: SimpleNameSyntax)
    case QualifiedName(left: NameSyntax, dotToken: SyntaxToken, right: SimpleNameSyntax)
}

case class ExpressionItemSyntax(expression: Expression, separatorToken: Option[SyntaxToken])
case class ExpressionListSyntax(expressions: List[ExpressionItemSyntax])

case class BlockExpressionListSyntax(
                                      statements: List[StatementSyntax],
                                      expression: Option[Expression])

case class ElseSyntax(elseKeyword: SyntaxToken, expression: Expression)

enum Expression {
    case ArrayCreationExpression(newKeyword: SyntaxToken,
                                 name: NameSyntax,
                                 openBracket: SyntaxToken,
                                 arrayRank: Option[Expression],
                                 closeBracket: SyntaxToken,
                                 initializer: Option[ArrayInitializerExpressionSyntax])
    case AssignmentExpression(left: Expression,
                               equals: SyntaxToken,
                               right: Expression)
    case BinaryExpression(left: Expression, operator: SyntaxToken, right: Expression)
    case BlockExpression(
                          openBrace: SyntaxToken,
                          block: BlockExpressionListSyntax,
                          closeBrace: SyntaxToken)
    case CallExpression(
                         name: Expression,
                         genericArguments: Option[TypeArgumentListSyntax],
                         openParen: SyntaxToken,
                         arguments: ExpressionListSyntax,
                         closeParen: SyntaxToken)
    case ForExpression(
                        forKeyword: SyntaxToken,
                        openParen: SyntaxToken,
                        identifier: SyntaxToken,
                        arrow: SyntaxToken,
                        fromExpr: Expression,
                        toKeyword: SyntaxToken,
                        toExpr: Expression,
                        closeParen: SyntaxToken,
                        body: Expression)
    case GroupExpression(
                          openParen: SyntaxToken,
                          expression: Expression,
                          closeParen: SyntaxToken)
    case IdentifierName(value: SimpleNameSyntax.IdentifierNameSyntax)
    case If(
                       ifKeyword: SyntaxToken,
                       openParen: SyntaxToken,
                       condition: Expression,
                       closeParen: SyntaxToken,
                       thenExpr: Expression,
                       elseExpr: Option[ElseSyntax])
    case IndexExpression(
                          left: Expression,
                          openBracket: SyntaxToken,
                          index: Expression,
                          closeBracket: SyntaxToken)
    case LiteralExpression(token: SyntaxToken, value: SyntaxTokenValue)
    case MemberAccessExpression(
                                 left: Expression,
                                 dotToken: SyntaxToken,
                                 right: SimpleNameSyntax.IdentifierNameSyntax)
    case MatchExpression(expression: Expression, matchKeyword: SyntaxToken, openBrace: SyntaxToken, cases : Array[MatchCaseSyntax], closeBrace: SyntaxToken)
    case NewExpression(
                        newKeyword: SyntaxToken,
                        name: NameSyntax,
                        openParen: SyntaxToken,
                        arguments: ExpressionListSyntax,
                        closeParen: SyntaxToken)
    case UnaryExpression(operator: SyntaxToken, expression: Expression)
    case UnitExpression(openParen: SyntaxToken, closeParen: SyntaxToken)
    case WhileExpression(whileKeyword: SyntaxToken,
                         openParen: SyntaxToken,
                         condition: Expression,
                         closeParen: SyntaxToken,
                         body: Expression)
}

case class MatchCaseSyntax(
    caseKeyword: SyntaxToken,
    pattern: PatternSyntax,
    arrow: SyntaxToken,
    block: BlockExpressionListSyntax
)

enum PatternSyntax {
    case ExtractPattern(value: NameSyntax, openParenToken: SyntaxToken, patterns: Array[PatternItemSyntax], closeParenToken: SyntaxToken)
    case TypePattern(typ: NameSyntax)

    case IdentifierPattern(value: SyntaxToken, typeAnnotation: TypeAnnotationSyntax)
    case LiteralPattern(value: SyntaxToken)
    case DiscardPattern(value: SyntaxToken)
}

case class PatternItemSyntax(pattern: PatternSyntax, separatorToken: Option[SyntaxToken])

case class FunctionBodySyntax(equalToken: SyntaxToken, expression: Expression)
case class ParameterSyntax(
    modifier: Option[SyntaxToken],
    identifier: SyntaxToken,
    typeAnnotation: TypeAnnotationSyntax,
    commaToken: Option[SyntaxToken]
)

case class TypeArgumentItemSyntax(name: NameSyntax, separator: Option[SyntaxToken])

case class TypeArgumentListSyntax(
    lessThanToken: SyntaxToken,
    arguments: Array[TypeArgumentItemSyntax],
    greaterThanToken: SyntaxToken
)

case class UsingDirectiveSyntax(usingKeyword: SyntaxToken, name: NameSyntax)

case class TemplateSyntax(
    openBrace: SyntaxToken,
    members: List[MemberSyntax],
    closeBrace: SyntaxToken
)

enum StatementSyntax {
    case VariableDeclarationStatement(
                                       valOrVarKeyword: SyntaxToken,
                                       identifier: SyntaxToken,
                                       typeAnnotation: Option[TypeAnnotationSyntax],
                                       equalToken: SyntaxToken,
                                       expression: Expression)
    case BreakStatement(breakKeyword: SyntaxToken)
    case ContinueStatement(continueKeyword: SyntaxToken)
    case ExpressionStatement(expression: Expression)
}

case class EnumCaseParametersSyntax(openParenToken: SyntaxToken, parameters: List[ParameterSyntax], closeParenToken: SyntaxToken)
case class EnumCaseSyntax(caseKeyword: SyntaxToken, identifier: SyntaxToken, parameters: Option[EnumCaseParametersSyntax])

case class GenericBoundsSyntax(token: SyntaxToken, name: NameSyntax)
case class GenericParameterSyntax(variance: Option[SyntaxToken], identifier: SyntaxToken, bounds: Option[GenericBoundsSyntax])
case class GenericParametersSyntax(lessThanToken: SyntaxToken, parameters: SeparatedSyntaxList[GenericParameterSyntax], greaterThanToken: SyntaxToken)

case class SeparatedSyntaxList[T](items: List[T], separators: List[SyntaxToken])

enum MemberSyntax {
    case ObjectDeclarationSyntax(objectKeyword: SyntaxToken,
                                 identifier: SyntaxToken,
                                 template: TemplateSyntax)
    case ClassDeclarationSyntax(caseKeyword: Option[SyntaxToken],
                                classKeyword: SyntaxToken,
                                identifier: SyntaxToken,
                                genericParameters: Option[GenericParametersSyntax],
                                openParenToken: SyntaxToken,
                                parameters: List[ParameterSyntax],
                                closeParenToken: SyntaxToken,
                                template: Option[TemplateSyntax])
    case FunctionDeclarationSyntax(defKeyword: SyntaxToken,
                                   identifier: SyntaxToken,
                                   genericParameters: Option[GenericParametersSyntax],
                                   openParenToken: SyntaxToken,
                                   parameters: List[ParameterSyntax],
                                   closeParenToken: SyntaxToken,
                                   typeAnnotation: Option[TypeAnnotationSyntax],
                                   body: Option[FunctionBodySyntax])
    case GlobalStatementSyntax(statement: StatementSyntax)
    case EnumDeclarationSyntax(enumKeyword: SyntaxToken,
                               identifier: SyntaxToken,
                               genericParameters: Option[GenericParametersSyntax],
                               openBraceToken: SyntaxToken,
                               cases: Array[EnumCaseSyntax],
                               members: List[MemberSyntax],
                               closeBraceToken: SyntaxToken)
    case VariableDeclaration(valOrVarKeyword: SyntaxToken,
                             identifier: SyntaxToken,
                             typeAnnotation: Option[TypeAnnotationSyntax],
                             equalToken: SyntaxToken,
                             expression: Expression)
}

/**
  * names [1..n]
  */
case class NamespaceDeclarationSyntax(namespaceKeyword: SyntaxToken, name: NameSyntax)


/**
    namespaceDeclaration [0..1]
    usings [0..n]
    members [0..n]
*/
case class CompilationUnitSyntax(namespaceDeclaration: Option[NamespaceDeclarationSyntax], usings: Array[UsingDirectiveSyntax], members: List[MemberSyntax], endToken: SyntaxToken) {
    val kind: int = SyntaxKind.CompilationUnit
}

object AstUtils {
  def locationOfMember(member: MemberSyntax): TextLocation = {
    member match {
      case member: MemberSyntax.ObjectDeclarationSyntax =>
        member.objectKeyword.location.merge(
          member.template.closeBrace.location
        )
      case member: MemberSyntax.ClassDeclarationSyntax =>
        member.classKeyword.location.merge(
          member.template match {
            case Option.Some(value) => value.closeBrace.location
            case Option.None => member.closeParenToken.location
          }
        )
      case member: MemberSyntax.FunctionDeclarationSyntax =>
        member.defKeyword.location.merge(
          member.body match {
            case Option.Some(value) => locationOfExpression(value.expression)
            case Option.None => member.closeParenToken.location
          }
        )
      case member: MemberSyntax.EnumDeclarationSyntax =>
        member.enumKeyword.location.merge(
          member.closeBraceToken.location
        )
      case member: MemberSyntax.VariableDeclaration =>
        member.valOrVarKeyword.location.merge(
          locationOfExpression(member.expression)
        )
      case member: MemberSyntax.GlobalStatementSyntax => locationOfStatement(member.statement)
    }
  }

  def locationOfStatement(statement: StatementSyntax): TextLocation = {
    statement match {
      case statement: StatementSyntax.BreakStatement =>
        statement.breakKeyword.location
      case statement: StatementSyntax.ContinueStatement =>
        statement.continueKeyword.location
      case statement: StatementSyntax.ExpressionStatement =>
        locationOfExpression(statement.expression)
      case statement: StatementSyntax.VariableDeclarationStatement =>
        statement.valOrVarKeyword.location.merge(
          locationOfExpression(statement.expression)
        )
    }
  }

  def locationOfSimpleName(value: SimpleNameSyntax): TextLocation = {
    value match {
      case SimpleNameSyntax.GenericNameSyntax(identifier, typeArgumentlist) =>
        identifier.location.merge(typeArgumentlist.greaterThanToken.location)
      case SimpleNameSyntax.IdentifierNameSyntax(value) =>
        value.location
      case SimpleNameSyntax.ScalaAliasSyntax(open, name, arrow, alias, close) =>
        open.location.merge(close.location)
      case SimpleNameSyntax.AliasSyntax(name, asKeyword, alias) =>
        name.location.merge(alias.location)
    }
  }
  
  def locationOfName(value: NameSyntax): TextLocation = {
    value match {
      case NameSyntax.QualifiedName(left, dotToken, right) =>
        locationOfName(left).merge(locationOfSimpleName(right))
      case NameSyntax.SimpleName(name) =>
        name match {
          case SimpleNameSyntax.GenericNameSyntax(identifier, typeArgumentlist) =>
            identifier.location.merge(typeArgumentlist.greaterThanToken.location)
          case SimpleNameSyntax.IdentifierNameSyntax(value) =>
            value.location
          case SimpleNameSyntax.ScalaAliasSyntax(open, name, arrow, alias, close) =>
            open.location.merge(close.location)
          case SimpleNameSyntax.AliasSyntax(name, asKeyword, alias) =>
            name.location.merge(alias.location)
        }
    }
  }
  
  def locationOfExpression(value: Expression): TextLocation = {
    value match {
      case Expression.ArrayCreationExpression(
      newKeyword,
      _,
      _,
      _,
      closeBracket,
      initializer
      ) =>
        val end = initializer match {
          case Option.Some(value) => value.closeBrace.location
          case Option.None => closeBracket.location
        }
        newKeyword.location.merge(end)
      case Expression.AssignmentExpression(left, equals, right) =>
        locationOfExpression(left).merge(locationOfExpression(right))
      case Expression.BinaryExpression(left, operator, right) =>
        locationOfExpression(left).merge(locationOfExpression(right))
      case Expression.BlockExpression(openBrace, block, closeBrace) =>
        openBrace.location.merge(closeBrace.location)
      case Expression.CallExpression(name, _, _, _, closeParen) =>
        locationOfExpression(name).merge(closeParen.location)
      case Expression.ForExpression(forKeyword, _, _, _, _, _, _, _, body) =>
        forKeyword.location.merge(locationOfExpression(body))
      case Expression.GroupExpression(openParen, expression, closeParen) =>
        openParen.location.merge(closeParen.location)
      case Expression.IdentifierName(value) =>
        value.identifier.location
      case Expression.If(ifKeyword, _, _, _, expression, elseExpr) =>
        ifKeyword.location.merge(locationOfExpression(elseExpr match {
          case Option.Some(value) => value.expression
          case Option.None => expression
        }))
      case Expression.IndexExpression(left, openBracket, index, closeBracket) =>
        locationOfExpression(left).merge(closeBracket.location)
      case Expression.LiteralExpression(token, value) =>
        token.location
      case Expression.MemberAccessExpression(left, dotToken, right) =>
        locationOfExpression(left).merge(right.identifier.location)
      case Expression.MatchExpression(expression, _, _, _, closeBrace) =>
        locationOfExpression(expression).merge(closeBrace.location)
      case Expression.NewExpression(newKeyword, _, _, _, closeParen) =>
        newKeyword.location.merge(closeParen.location)
      case Expression.UnaryExpression(operator, expression) =>
        operator.location.merge(locationOfExpression(expression))
      case Expression.UnitExpression(openParen, closeParen) =>
        openParen.location.merge(closeParen.location)
      case Expression.WhileExpression(whileKeyword, _, _, _, body) =>
        whileKeyword.location.merge(locationOfExpression(body))
    }
  }

  def locationOfBoundExpression(value: BoundExpression): TextLocation = {
    value match {
      case BoundExpression.Assignment(location, _, expression) => location.merge(locationOfBoundExpression(expression))
      case BoundExpression.BinaryExpression(location, left, _, right, _) => locationOfBoundExpression(left).merge(locationOfBoundExpression(right))
      case BoundExpression.Block(statements, expression) => locationOfBoundExpression(expression)
      case BoundExpression.BooleanLiteral(location, _) => location
      case BoundExpression.CallExpression(location, _, _, _, _) => location
      case BoundExpression.CastExpression(location, expression, _) => location
      case BoundExpression.CharacterLiteral(location, _) => location
      case BoundExpression.ForExpression(location, _, _, _, _) => location
      case BoundExpression.IfExpression(location, cond, thenExpr, elseExpr, _) =>
        locationOfBoundExpression(thenExpr).merge(
          elseExpr match {
            case Option.Some(value) => locationOfBoundExpression(value)
            case Option.None => locationOfBoundExpression(thenExpr)
          }
        )
      case BoundExpression.IndexExpression(location, _, _, _) => location
      case BoundExpression.IntLiteral(location, _) => location
      case BoundExpression.MemberAccess(location, _, _, _) => location
      case BoundExpression.NewExpression(location, _, _, _, _) => location
      case BoundExpression.StringLiteral(location, _) => location
      case BoundExpression.UnaryExpression(location, _, operand, _) => location.merge(locationOfBoundExpression(operand))
      case BoundExpression.UnitExpression(location) => location
      case BoundExpression.Variable(location, _, _) => location
      case BoundExpression.WhileExpression(location, _, _) => location

      case BoundExpression.Error => TextLocationFactory.empty()
    }
  }
}