package minijava.typechecking

import minijava.grammar._
import minijava.messages.{LineColumn, LineNumber, Location}

import scala.collection.mutable.ArrayBuffer

case class IOInstance(location: Location, node: ASTNode)

class IOCheckingVisitor extends ASTVisitor[Unit, Unit] {
  val ioInstances = new ArrayBuffer[IOInstance]()

  def getIOInstances(): List[IOInstance] = {
    ioInstances.toList
  }

  override def visitStatementBlock(statementBlock: StatementBlock, a: Unit): Unit = {
    statementBlock.statements.foreach(visit(_, a))
  }

  override def visitIfStatement(ifStatement: IfStatement, a: Unit): Unit = {
    visit(ifStatement.condition, a)
    visit(ifStatement.thenClause, a)
    visit(ifStatement.elseClause, a)
  }

  override def visitWhileStatement(whileStatement: WhileStatement, a: Unit): Unit = {
    visit(whileStatement.condition, a)
    visit(whileStatement.statement, a)
  }

  override def visitPrintStatement(printStatement: PrintStatement, a: Unit): Unit = {
    // Print statements are always io, since they print to stdout
    ioInstances.append(IOInstance(
      LineNumber(printStatement.line),
      printStatement
    ))
  }

  override def visitAssignmentStatement(assignmentStatement: AssignmentStatement, a: Unit): Unit = {
    visit(assignmentStatement.expression, a)
  }

  override def visitArrayAssignmentStatement(arrayAssignmentStatement: ArrayAssignmentStatement, a: Unit): Unit = {
    visit(arrayAssignmentStatement.valueExpression, a)
    visit(arrayAssignmentStatement.indexExpression, a)
  }

  override def visitBinaryOperationExpression(binaryOperationExpression: BinaryOperationExpression, a: Unit): Unit = {
    visit(binaryOperationExpression.firstExpression, a)
    visit(binaryOperationExpression.secondExpression, a)
  }

  override def visitArrayAccessExpression(arrayAccessExpression: ArrayAccessExpression, a: Unit): Unit = {
    visit(arrayAccessExpression.indexExpression, a)
    visit(arrayAccessExpression.arrayExpression, a)
  }

  override def visitMethodCallExpression(methodCallExpression: MethodCallExpression, a: Unit): Unit = {
    // A method call is only io if the method being called is known to perform io
    if (methodCallExpression.method.get.isIO) {
      ioInstances.append(IOInstance(
        LineColumn(methodCallExpression.line, methodCallExpression.column),
        methodCallExpression
      ))
    }
  }

  override def visitArrayLengthExpression(arrayLengthExpression: ArrayLengthExpression, a: Unit): Unit = {
    // Getting the length of an array never causes io
  }

  override def visitIntegerLiteral(integerLiteral: IntegerLiteral, a: Unit): Unit = {
    // Using an integer literal never causes io
  }

  override def visitTrueLiteral(a: Unit): Unit = {
    // Using a true literal never causes io
  }

  override def visitFalseLiteral(a: Unit): Unit = {
    // Using a false literal never causes io
  }

  override def visitIdentifierExpression(identifierExpression: IdentifierExpression, a: Unit): Unit = {
    // Reading a variable never causes io
  }

  override def visitThisLiteral(a: Unit): Unit = {
    // Using the current object "this" literal never causes io
  }

  override def visitNewIntArrayExpression(intArrayExpression: NewIntArrayExpression, a: Unit): Unit = {
    visit(intArrayExpression.lengthExpression, a)
  }

  override def visitNewObjectExpression(newObjectExpression: NewObjectExpression, a: Unit): Unit = {
    // An object instantiation is never io, since Minijava does not allow custom constructors
  }

  override def visitNegatedExpression(negatedExpression: NegatedExpression, a: Unit): Unit = {
    visit(negatedExpression.expression, a)
  }

  override def visitParenedExpression(parenedExpression: ParenedExpression, a: Unit): Unit = {
    visit(parenedExpression.expression, a)
  }
}
