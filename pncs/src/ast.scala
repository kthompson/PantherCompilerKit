import panther._

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

case class IdentifierNameSyntax(identifier: SyntaxToken)
case class GenericNameSyntax(
    identifier: SyntaxToken,
    typeArgumentlist: TypeArgumentListSyntax
)
case class QualifiedNameSyntax(left: NameSyntax, dotToken: SyntaxToken, right: SimpleNameSyntax)

case class SimpleNameSyntax(
    kind: int,
    genericName: Option[GenericNameSyntax],
    identifierName: Option[IdentifierNameSyntax]
)
case class NameSyntax(
    kind: int,
    qualifiedName: Option[QualifiedNameSyntax],
    simpleName: Option[SimpleNameSyntax]
)
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
    statements: Array[StatementSyntax],
    expression: Option[ExpressionSyntax], /* 0 to 1 */
    closeBrace: SyntaxToken
)
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
)
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
    right: IdentifierNameSyntax
)
case class NewExpressionSyntax(
    newKeyword: SyntaxToken,
    identifier: NameSyntax,
    openParen: SyntaxToken,
    arguments: ExpressionListSyntax,
    closeParen: SyntaxToken
)
case class UnaryExpressionSyntax(operator: SyntaxToken, expression: ExpressionSyntax)
case class UnitExpressionSyntax(openParen: SyntaxToken, closeParen: SyntaxToken)
case class WhileExpressionSyntax(
    whileKeyword: SyntaxToken,
    openParen: SyntaxToken,
    left: ExpressionSyntax,
    closeParen: SyntaxToken,
    body: ExpressionSyntax
)

case class ExpressionSyntax(
    kind: int,
    arrayCreationExpression: Option[ArrayCreationExpressionSyntax],
    assignmentExpression: Option[AssignmentExpressionSyntax],
    binaryExpression: Option[BinaryExpressionSyntax],
    blockExpression: Option[BlockExpressionSyntax],
    callExpression: Option[CallExpressionSyntax],
    forExpression: Option[ForExpressionSyntax],
    groupExpression: Option[GroupExpressionSyntax],
    identifierName: Option[IdentifierNameSyntax],
    ifExpression: Option[IfExpressionSyntax],
    indexExpression: Option[IndexExpressionSyntax],
    literalExpression: Option[LiteralExpressionSyntax],
    memberAccessExpression: Option[MemberAccessExpressionSyntax],
    newExpression: Option[NewExpressionSyntax],
    unaryExpression: Option[UnaryExpressionSyntax],
    unitExpression: Option[UnitExpressionSyntax],
    whileExpression: Option[WhileExpressionSyntax]
) {
    var id = -1 // used in type checker
}

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

case class ObjectDeclarationSyntax(
    objectKeyword: SyntaxToken,
    identifier: SyntaxToken,
    template: TemplateSyntax
)

case class ClassDeclarationSyntax(
    classKeyword: SyntaxToken,
    identifier: SyntaxToken,
    openParenToken: SyntaxToken,
    parameters: Array[ParameterSyntax],
    closeParenToken: SyntaxToken,
    template: Option[TemplateSyntax]
)

case class FunctionDeclarationSyntax(
    defKeyword: SyntaxToken,
    identifier: SyntaxToken,
    openParenToken: SyntaxToken,
    parameters: Array[ParameterSyntax],
    closeParenToken: SyntaxToken,
    typeAnnotation: Option[TypeAnnotationSyntax],
    body: Option[FunctionBodySyntax]
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

case class StatementSyntax(
    kind: int,
    variableDeclarationStatement: Option[VariableDeclarationStatementSyntax],
    breakStatement: Option[BreakStatementSyntax],
    continueStatement: Option[ContinueStatementSyntax],
    expressionStatement: Option[ExpressionStatementSyntax]
)

case class GlobalStatementSyntax(statement: StatementSyntax)

case class MemberSyntax(
                         kind: int,
                         objekt: Option[ObjectDeclarationSyntax],
                         klass: Option[ClassDeclarationSyntax],
                         function: Option[FunctionDeclarationSyntax],
                         statement: Option[GlobalStatementSyntax]
)

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
    val Object = 1
    val Class = 2
    val Method = 3
    val Parameter = 4
    val Local = 5
    val Token = 6
}

// A declaration is any syntax that defines a binding, the type of the
// symbol will not always match the declarations.
// For example, a constructor symbol will usually have a Class declaration
// and field symbols can either be Local declarations or Parameter declarations
case class Declaration(
    kind: int,
    name: string,
    location: TextLocation,
    object_declaration: Option[ObjectDeclarationSyntax],
    class_declaration: Option[ClassDeclarationSyntax],
    function_declaration: Option[FunctionDeclarationSyntax],
    parameter_declaration: Option[ParameterSyntax],
    local_declaration: Option[VariableDeclarationStatementSyntax],
    token_declaration: Option[SyntaxToken]
) {
    var id = -1 // used in type checker
}

object MakeDeclaration {
    def obj(node: ObjectDeclarationSyntax): Declaration =
        new Declaration(
            DeclarationKind.Object,
            node.identifier.text,
            node.identifier.location,
            Some(node),
            None,
            None,
            None,
            None,
            None
        )

    def cls(node: ClassDeclarationSyntax): Declaration =
        new Declaration(
            DeclarationKind.Class,
            node.identifier.text,
            node.identifier.location,
            None,
            Some(node),
            None,
            None,
            None,
            None
        )

    def function(node: FunctionDeclarationSyntax): Declaration =
        new Declaration(
            DeclarationKind.Method,
            node.identifier.text,
            node.identifier.location,
            None,
            None,
            Some(node),
            None,
            None,
            None
        )

    def parameter(node: ParameterSyntax): Declaration =
        new Declaration(
            DeclarationKind.Parameter,
            node.identifier.text,
            node.identifier.location,
            None,
            None,
            None,
            Some(node),
            None,
            None
        )

    def local(node: VariableDeclarationStatementSyntax): Declaration =
        new Declaration(
            DeclarationKind.Local,
            node.identifier.text,
            node.identifier.location,
            None,
            None,
            None,
            None,
            Some(node),
            None
        )

    def token(node: SyntaxToken): Declaration =
        new Declaration(
            DeclarationKind.Local,
            node.text,
            node.location,
            None,
            None,
            None,
            None,
            None,
            Some(node)
        )
}


// A Node represents any piece of the AST
case class Node(
    declaration: Option[Declaration],
    expression: Option[ExpressionSyntax]
) {

    def is_decl(): bool = declaration.isDefined

    def id(): int =
        if (is_decl()) declaration.get.id
        else expression.get.id

    def set_id(value: int): unit = {
        if (is_decl()) {
            declaration.get.id = value
        } else {
            expression.get.id = value
        }
    }
}

object MakeNode {
    def decl(declaration: Declaration): Node = new Node(Some(declaration), None)
    def expr(expression: ExpressionSyntax): Node = new Node(None, Some(expression))
}