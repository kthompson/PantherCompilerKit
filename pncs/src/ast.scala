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

case class ArrayCreationExpressionSyntax(
    newKeyword: SyntaxToken,
    name: NameSyntax,
    openBracket: SyntaxToken,
    arrayRank: Option[ExpressionSyntax],
    closeBracket: SyntaxToken,
    initializer: Option[ArrayInitializerExpressionSyntax]
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
case class ExpressionItemSyntax(expression: ExpressionSyntax, separatorToken: Option[SyntaxToken])
case class ExpressionListSyntax(expressions: Array[ExpressionItemSyntax])

case class AssignmentExpressionSyntax(
    left: ExpressionSyntax,
    equals: SyntaxToken,
    right: ExpressionSyntax
)
case class BinaryExpressionSyntax(left: ExpressionSyntax, operator: SyntaxToken, right: ExpressionSyntax)
case class BlockExpressionSyntax(
    openBrace: SyntaxToken,
    block: BlockExpressionListSyntax,
    closeBrace: SyntaxToken
) {
    var id = -1 // used in type checker, set in binder
}
case class BlockExpressionListSyntax(
                                      statements: Array[StatementSyntax],
                                      expression: Option[ExpressionSyntax])
case class CallExpressionSyntax(
    name: ExpressionSyntax,
    openParen: SyntaxToken,
    arguments: ExpressionListSyntax,
    closeParen: SyntaxToken
)
case class ForExpressionSyntax(
    forKeyword: SyntaxToken,
    openParen: SyntaxToken,
    identifier: SyntaxToken,
    arrow: SyntaxToken,
    fromExpr: ExpressionSyntax,
    toKeyword: SyntaxToken,
    toExpr: ExpressionSyntax,
    closeParen: SyntaxToken,
    body: ExpressionSyntax
) {
    var id = -1 // used in type checker, set in binder
}
case class GroupExpressionSyntax(
    openParen: SyntaxToken,
    expression: ExpressionSyntax,
    closeParen: SyntaxToken
)
case class IfExpressionSyntax(
                               ifKeyword: SyntaxToken,
                               openParen: SyntaxToken,
                               condition: ExpressionSyntax,
                               closeParen: SyntaxToken,
                               thenExpr: ExpressionSyntax,
                               elseKeyword: SyntaxToken,
                               elseExpr: ExpressionSyntax
)
case class IndexExpressionSyntax(
    left: ExpressionSyntax,
    openBracket: SyntaxToken,
    index: ExpressionSyntax,
    closeBracket: SyntaxToken
)
case class LiteralExpressionSyntax(token: SyntaxToken, value: SyntaxTokenValue)
case class MemberAccessExpressionSyntax(
    left: ExpressionSyntax,
    dotToken: SyntaxToken,
    right: SimpleNameSyntax.IdentifierNameSyntax
)
case class NewExpressionSyntax(
                                newKeyword: SyntaxToken,
                                name: NameSyntax,
                                openParen: SyntaxToken,
                                arguments: ExpressionListSyntax,
                                closeParen: SyntaxToken
)
case class UnaryExpressionSyntax(operator: SyntaxToken, expression: ExpressionSyntax)
case class UnitExpressionSyntax(openParen: SyntaxToken, closeParen: SyntaxToken)
case class WhileExpressionSyntax(
                                  whileKeyword: SyntaxToken,
                                  openParen: SyntaxToken,
                                  condition: ExpressionSyntax,
                                  closeParen: SyntaxToken,
                                  body: ExpressionSyntax
)

enum ExpressionSyntax {
    case ArrayCreationExpression(value: ArrayCreationExpressionSyntax)
    case AssignmentExpression(value: AssignmentExpressionSyntax)
    case BinaryExpression(value: BinaryExpressionSyntax)
    case BlockExpression(value: BlockExpressionSyntax)
    case CallExpression(value: CallExpressionSyntax)
    case ForExpression(value: ForExpressionSyntax)
    case GroupExpression(value: GroupExpressionSyntax)
    case IdentifierName(value: SimpleNameSyntax.IdentifierNameSyntax)
    case IfExpression(value: IfExpressionSyntax)
    case IndexExpression(value: IndexExpressionSyntax)
    case LiteralExpression(value: LiteralExpressionSyntax)
    case MemberAccessExpression(value: MemberAccessExpressionSyntax)
    case MatchExpression(expression: ExpressionSyntax, matchKeyword: SyntaxToken, openBrace: SyntaxToken, cases : Array[MatchCaseSyntax], closeBrace: SyntaxToken)
    case NewExpression(value: NewExpressionSyntax)
    case UnaryExpression(value: UnaryExpressionSyntax)
    case UnitExpression(value: UnitExpressionSyntax)
    case WhileExpression(value: WhileExpressionSyntax)
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

case class FunctionBodySyntax(equalToken: SyntaxToken, expression: ExpressionSyntax)
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

case class VariableDeclarationStatementSyntax(
    valOrVarKeyword: SyntaxToken,
    identifier: SyntaxToken,
    typeAnnotation: Option[TypeAnnotationSyntax],
    equalToken: SyntaxToken,
    expression: ExpressionSyntax
)
case class BreakStatementSyntax(breakKeyword: SyntaxToken)
case class ContinueStatementSyntax(continueKeyword: SyntaxToken)
case class ExpressionStatementSyntax(expression: ExpressionSyntax)

enum StatementSyntax {
    case VariableDeclarationStatement(value: VariableDeclarationStatementSyntax)
    case BreakStatement(value: BreakStatementSyntax)
    case ContinueStatement(value: ContinueStatementSyntax)
    case ExpressionStatement(value: ExpressionStatementSyntax)
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
    case FieldFromVariable(name: string, location: TextLocation, value: VariableDeclarationStatementSyntax)
    
    case Parameter(name: string, location: TextLocation, value: ParameterSyntax)
    case Local(name: string, location: TextLocation, value: VariableDeclarationStatementSyntax)
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