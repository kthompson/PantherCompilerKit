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
    case QualifiedNameSyntax(left: NameSyntax, dotToken: SyntaxToken, right: SimpleNameSyntax)
    case SimpleName(name: SimpleNameSyntax)
}
case class ExpressionItemSyntax(expression: Expression, separatorToken: Option[SyntaxToken])
case class ExpressionListSyntax(expressions: Array[ExpressionItemSyntax])

case class BlockExpressionListSyntax(
                                      statements: Array[StatementSyntax],
                                      expression: Option[Expression])


enum Expression {
    case ArrayCreationExpression(newKeyword: SyntaxToken,
                                 name: NameSyntax,
                                 openBracket: SyntaxToken,
                                 arrayRank: Option[Expression],
                                 closeBracket: SyntaxToken,
                                 initializer: Option[ArrayInitializerExpressionSyntax])
    case AssignmentExpression(
                               left: Expression,
                               equals: SyntaxToken,
                               right: Expression)
    case BinaryExpression(left: Expression, operator: SyntaxToken, right: Expression)
    case BlockExpression(
                          openBrace: SyntaxToken,
                          block: BlockExpressionListSyntax,
                          closeBrace: SyntaxToken)
    case CallExpression(
                         name: Expression,
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
    case IfExpression(
                       ifKeyword: SyntaxToken,
                       openParen: SyntaxToken,
                       condition: Expression,
                       closeParen: SyntaxToken,
                       thenExpr: Expression,
                       elseKeyword: SyntaxToken,
                       elseExpr: Expression)
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
    case IdentifierPattern(value: SyntaxToken, typeAnnotation: TypeAnnotationSyntax)
    case TypePattern(typ: NameSyntax)
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
    members: Array[MemberSyntax],
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

case class EnumCaseParametersSyntax(openParenToken: SyntaxToken, parameters: Array[ParameterSyntax], closeParenToken: SyntaxToken)
case class EnumCaseSyntax(caseKeyword: SyntaxToken, identifier: SyntaxToken, parameters: Option[EnumCaseParametersSyntax])

enum MemberSyntax {
    case ObjectDeclarationSyntax(objectKeyword: SyntaxToken,
                                 identifier: SyntaxToken,
                                 template: TemplateSyntax)
    case ClassDeclarationSyntax(caseKeyword: Option[SyntaxToken],
                                classKeyword: SyntaxToken,
                                identifier: SyntaxToken,
                                openParenToken: SyntaxToken,
                                parameters: Array[ParameterSyntax],
                                closeParenToken: SyntaxToken,
                                template: Option[TemplateSyntax])
    case FunctionDeclarationSyntax(defKeyword: SyntaxToken,
                                   identifier: SyntaxToken,
                                   openParenToken: SyntaxToken,
                                   parameters: Array[ParameterSyntax],
                                   closeParenToken: SyntaxToken,
                                   typeAnnotation: Option[TypeAnnotationSyntax],
                                   body: Option[FunctionBodySyntax])
    case GlobalStatementSyntax(statement: StatementSyntax)
    case EnumDeclarationSyntax(enumKeyword: SyntaxToken,
                               identifier: SyntaxToken,
                               openBraceToken: SyntaxToken,
                               cases: Array[EnumCaseSyntax],
                               members: Array[MemberSyntax],
                               closeBraceToken: SyntaxToken)
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
case class CompilationUnitSyntax(namespaceDeclaration: Option[NamespaceDeclarationSyntax], usings: Array[UsingDirectiveSyntax], members: Array[MemberSyntax], endToken: SyntaxToken) {
    val kind: int = SyntaxKind.CompilationUnit
}


object DeclarationKind {
    val Member = 1
    val Parameter = 2
    val Local = 3
    val Token = 4
    val Builtin = 5
}

// A declaration is any syntax that defines a binding, the type of the
// symbol will not always match the declarations.
// For example, a constructor symbol will usually have a Class declaration
// and field symbols can either be Local declarations or Parameter declarations

enum Declaration {
    case Class(name: string, location: TextLocation, value: MemberSyntax.ClassDeclarationSyntax)
    case ClassFromObject(name: string, location: TextLocation, value: MemberSyntax.ObjectDeclarationSyntax)
    case ClassFromEnum(name: string, location: TextLocation, value: MemberSyntax.EnumDeclarationSyntax)
    case ClassFromEnumCase(name: string, location: TextLocation, value: EnumCaseSyntax)

    case Constructor(name: string, location: TextLocation, value: Array[ParameterSyntax])

    case Method(name: string, location: TextLocation, value: MemberSyntax.FunctionDeclarationSyntax)

    case FieldFromParameter(name: string, location: TextLocation, value: ParameterSyntax)
    case FieldFromVariable(name: string, location: TextLocation, value: StatementSyntax.VariableDeclarationStatement)

    case Parameter(name: string, location: TextLocation, value: ParameterSyntax)
    case Local(name: string, location: TextLocation, value: StatementSyntax.VariableDeclarationStatement)
}
//case class Declaration(
//    kind: int,
//    name: string,
//    location: TextLocation,
//    memberDeclaration: Option[MemberSyntax],
//    enumCaseSyntax: Option[EnumCaseSyntax],
//    parameter_declaration: Option[ParameterSyntax],
//    local_declaration: Option[VariableDeclarationStatementSyntax],
//    token_declaration: Option[SyntaxToken]
//) {
//    var id = -1 // used in type checker
//}
//
//object MakeDeclaration {
//    def builtin(name: string): Declaration =
//        new Declaration(
//            DeclarationKind.Builtin,
//            name,
//            TextLocation(new SourceFile("", ""), TextSpan(0, 0)),
//            None,
//            None,
//            None,
//            None,
//            None
//        )
//
//    def enumCaseSyntax(node: EnumCaseSyntax): Declaration = {
//        new Declaration(
//            DeclarationKind.Member,
//            node.identifier.text,
//            node.identifier.location,
//            None,
//            Some(node),
//            None,
//            None,
//            None
//        )
//    }
//
//    def member(identifier: SyntaxToken, node: MemberSyntax): Declaration = {
//        new Declaration(
//            DeclarationKind.Member,
//            identifier.text,
//            identifier.location,
//            Some(node),
//            None,
//            None,
//            None,
//            None
//        )
//    }
//
//    def parameter(node: ParameterSyntax): Declaration =
//        new Declaration(
//            DeclarationKind.Parameter,
//            node.identifier.text,
//            node.identifier.location,
//            None,
//            None,
//            Some(node),
//            None,
//            None
//        )
//
//    def local(node: VariableDeclarationStatementSyntax): Declaration =
//        new Declaration(
//            DeclarationKind.Local,
//            node.identifier.text,
//            node.identifier.location,
//            None,
//            None,
//            None,
//            Some(node),
//            None
//        )
//
//    def token(node: SyntaxToken): Declaration =
//        new Declaration(
//            DeclarationKind.Local,
//            node.text,
//            node.location,
//            None,
//            None,
//            None,
//            None,
//            Some(node)
//        )
//}

//
//// A Node represents any piece of the AST
//case class Node(
//    declaration: Option[Declaration],
//    expression: Option[ExpressionSyntax]
//) {
//
//    def is_decl(): bool = declaration.isDefined
//
//    def id(): int =
//        if (is_decl()) declaration.get.id
//        else expression.get.id
//
//    def set_id(value: int): unit = {
//        if (is_decl()) {
//            declaration.get.id = value
//        } else {
//            expression.get.id = value
//        }
//    }
//}

//object MakeNode {
//    def decl(declaration: Declaration): Node = new Node(Some(declaration), None)
//    def expr(expression: ExpressionSyntax): Node = new Node(None, Some(expression))
//}