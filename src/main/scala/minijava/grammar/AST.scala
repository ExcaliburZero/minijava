package minijava.grammar

sealed trait ASTNode

case class Goal(mainClass: MainClass, classDeclarations: List[ClassDeclaration]) extends ASTNode

case class MainClass(name: Identifier, isIO: Boolean, parameter: Identifier, statement: Statement) extends ASTNode

case class ClassDeclaration(name: Identifier, parentClass: Option[Identifier], variableDeclarations: List[VariableDeclaration], methodDeclarations: List[MethodDeclaration]) extends ASTNode

case class VariableDeclaration(varType: Type, name: Identifier) extends ASTNode

case class MethodDeclaration(isIO: Boolean, varType: Type, name: Identifier, parameters: List[(Type, Identifier)], variableDeclarations: List[VariableDeclaration], statements: List[Statement], returnExpression: Expression) extends ASTNode

sealed trait Type extends ASTNode
case object IntArrayType extends Type
case object BooleanType extends Type
case object IntType extends Type

sealed trait Statement extends ASTNode
case class StatementBlock(statements: List[Statement]) extends Statement
case class IfStatement(condition: Expression, thenClause: Statement, elseClause: Statement) extends Statement
case class WhileStatement(condition: Expression, statements: Statement) extends Statement
case class PrintStatement(expression: Expression) extends Statement
case class AssignmentStatement(name: Identifier, expression: Expression) extends Statement
case class ArrayAssignmentStatement(name: Identifier, indexExpression: Expression, valueExpression: Expression) extends Statement

sealed trait Expression extends ASTNode
case class BinaryOperationExpression(firstExpression: Expression, operator: BinaryOperator, secondExpression: Expression) extends Expression
case class ArrayAccessExpression(arrayExpression: Expression, indexExpression: Expression) extends Expression
case class ArrayLengthExpression(arrayExpression: Expression) extends Expression
case class MethodCallExpression(objectExpression: Expression, methodName: Identifier, parameters: List[Expression]) extends Expression
// TODO: more

sealed trait BinaryOperator
case object And extends BinaryOperator
case object LessThan extends BinaryOperator
case object Plus extends BinaryOperator
case object Minus extends BinaryOperator
case object Times extends BinaryOperator

case class Identifier(name: String) extends Type with Expression
