package minijava.typechecking

import minijava.grammar._
import minijava.messages.CompilerMessage

import scala.collection.mutable.ArrayBuffer

case class TypeVisitorContext(typeTable: TypeTable, curClass: ClassType, curMethod: Method)

class TypeCheckingVisitor extends ASTVisitor[TypeVisitorContext, TypeDefinition] {
  private val typeCheckingErrors = new ArrayBuffer[CompilerMessage]()

  def getTypeCheckingErrors(): List[CompilerMessage] = {
    typeCheckingErrors.toList
  }

  override def visitStatementBlock(statementBlock: StatementBlock, a: TypeVisitorContext): TypeDefinition = super.visitStatementBlock(statementBlock, a)

  override def visitIfStatement(ifStatement: IfStatement, a: TypeVisitorContext): TypeDefinition = super.visitIfStatement(ifStatement, a)

  override def visitWhileStatement(whileStatement: WhileStatement, a: TypeVisitorContext): TypeDefinition = super.visitWhileStatement(whileStatement, a)

  override def visitPrintStatement(printStatement: PrintStatement, a: TypeVisitorContext): TypeDefinition = super.visitPrintStatement(printStatement, a)

  override def visitAssignmentStatement(assignmentStatement: AssignmentStatement, a: TypeVisitorContext): TypeDefinition = {
    val expressionType = assignmentStatement.expression
    val variableType = ???

    ???
  }

  override def visitArrayAssignmentStatement(arrayAssignmentStatement: ArrayAssignmentStatement, a: TypeVisitorContext): TypeDefinition = super.visitArrayAssignmentStatement(arrayAssignmentStatement, a)

  override def visitBinaryOperationExpression(binaryOperationExpression: BinaryOperationExpression, a: TypeVisitorContext): TypeDefinition = super.visitBinaryOperationExpression(binaryOperationExpression, a)

  override def visitArrayAccessExpression(arrayAccessExpression: ArrayAccessExpression, a: TypeVisitorContext): TypeDefinition = super.visitArrayAccessExpression(arrayAccessExpression, a)

  override def visitArrayLengthExpression(arrayLengthExpression: ArrayLengthExpression, a: TypeVisitorContext): TypeDefinition = super.visitArrayLengthExpression(arrayLengthExpression, a)

  override def visitMethodCallExpression(methodCallExpression: MethodCallExpression, a: TypeVisitorContext): TypeDefinition = super.visitMethodCallExpression(methodCallExpression, a)

  override def visitIntegerLiteral(integerLiteral: IntegerLiteral, a: TypeVisitorContext): TypeDefinition = super.visitIntegerLiteral(integerLiteral, a)

  override def visitTrueLiteral(a: TypeVisitorContext): TypeDefinition = super.visitTrueLiteral(a)

  override def visitFalseLiteral(a: TypeVisitorContext): TypeDefinition = super.visitFalseLiteral(a)

  override def visitIdentifierExpression(identifierExpression: IdentifierExpression, a: TypeVisitorContext): TypeDefinition = super.visitIdentifierExpression(identifierExpression, a)

  override def visitThisLiteral(a: TypeVisitorContext): TypeDefinition = super.visitThisLiteral(a)

  override def visitNewIntArrayExpression(intArrayExpression: NewIntArrayExpression, a: TypeVisitorContext): TypeDefinition = super.visitNewIntArrayExpression(intArrayExpression, a)

  override def visitNewObjectExpression(newObjectExpression: NewObjectExpression, a: TypeVisitorContext): TypeDefinition = super.visitNewObjectExpression(newObjectExpression, a)

  override def visitNegatedExpression(negatedExpression: NegatedExpression, a: TypeVisitorContext): TypeDefinition = super.visitNegatedExpression(negatedExpression, a)

  override def visitParenedExpression(parenedExpression: ParenedExpression, a: TypeVisitorContext): TypeDefinition = super.visitParenedExpression(parenedExpression, a)

  def getVarType(name: String, context: TypeVisitorContext): TypeDefinition = {
    // TODO: somewhere check for shadowed names
    context.curMethod.parameters ++ context.curMethod.localVariables
    ???
  }
}
