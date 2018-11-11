package minijava.grammar

import minijava.typechecking.{Method, TypeDefinition, VariableContext}

sealed trait ASTNode

case class Goal(mainClass: MainClass, regularClasses: List[RegularClass]) extends ASTNode

sealed trait ClassDeclaration extends ASTNode
case class MainClass(name: Identifier, isIO: Boolean, parameter: Identifier, statement: Statement, line: Int) extends ClassDeclaration
case class RegularClass(name: Identifier, parentClass: Option[Identifier], variableDeclarations: List[VariableDeclaration], methodDeclarations: List[MethodDeclaration], line: Int) extends ClassDeclaration

case class VariableDeclaration(varType: Type, name: Identifier, line: Int) extends ASTNode

case class MethodDeclaration(isIO: Boolean, varType: Type, name: Identifier, parameters: List[(Type, Identifier)], variableDeclarations: List[VariableDeclaration], statements: List[Statement], returnExpression: Expression, line: Int) extends ASTNode

sealed trait Type extends ASTNode
case object IntArrayType extends Type
case object BooleanType extends Type
case object IntType extends Type
case class IdentifierType(name: Identifier) extends Type

sealed trait Statement extends ASTNode
case class StatementBlock(statements: List[Statement]) extends Statement
case class IfStatement(condition: Expression, thenClause: Statement, elseClause: Statement, line: Int) extends Statement
case class WhileStatement(condition: Expression, statement: Statement, line: Int) extends Statement
case class PrintStatement(expression: Expression, line: Int) extends Statement
case class AssignmentStatement(name: Identifier, expression: Expression, line: Int) extends Statement {
  var context: Option[VariableContext] = None
}
case class ArrayAssignmentStatement(name: Identifier, indexExpression: Expression, valueExpression: Expression, line: Int) extends Statement {
  var context: Option[VariableContext] = None
}

sealed trait Expression extends ASTNode
case class BinaryOperationExpression(firstExpression: Expression, operator: BinaryOperator, secondExpression: Expression, line: Int, column: Int) extends Expression
case class ArrayAccessExpression(arrayExpression: Expression, indexExpression: Expression, line: Int, column: Int) extends Expression
case class ArrayLengthExpression(arrayExpression: Expression) extends Expression
case class MethodCallExpression(objectExpression: Expression, methodName: Identifier, parameters: List[Expression], line: Int, column: Int) extends Expression {
  var classType: Option[TypeDefinition] = None
  var method: Option[Method] = None
}
case class IntegerLiteral(value: Int) extends Expression
case object TrueLiteral extends Expression
case object FalseLiteral extends Expression
case class IdentifierExpression(name: Identifier, line: Int, column: Int) extends Expression {
  var context: Option[VariableContext] = None
}
case object ThisLiteral extends Expression
case class NewIntArrayExpression(lengthExpression: Expression, line: Int, column: Int) extends Expression
case class NewObjectExpression(className: Identifier, line: Int, column: Int) extends Expression
case class NegatedExpression(expression: Expression, line: Int, column: Int) extends Expression
case class ParenedExpression(expression: Expression) extends Expression

sealed trait BinaryOperator {
  def toSymbol(): String = {
    this match {
      case And => "&&"
      case LessThan => "<"
      case Plus => "+"
      case Minus => "-"
      case Times => "*"
    }
  }
}
case object And extends BinaryOperator
case object LessThan extends BinaryOperator
case object Plus extends BinaryOperator
case object Minus extends BinaryOperator
case object Times extends BinaryOperator

case class Identifier(name: String)
