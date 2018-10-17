package minijava.typechecking

import minijava.grammar._

class IOCheckingVisitor extends ASTVisitor[TypeTable, Boolean] {
  override def visitStatementBlock(statementBlock: StatementBlock, a: TypeTable): Boolean = {
    anyContainIO(statementBlock.statements, a)
  }

  override def visitIfStatement(ifStatement: IfStatement, a: TypeTable): Boolean = {
    visit(ifStatement.thenClause, a) ||
      visit(ifStatement.elseClause, a)
  }

  override def visitWhileStatement(whileStatement: WhileStatement, a: TypeTable): Boolean = {
    visit(whileStatement.statement, a)
  }

  override def visitPrintStatement(printStatement: PrintStatement, a: TypeTable): Boolean = {
    true
  }

  override def visitAssignmentStatement(assignmentStatement: AssignmentStatement, a: TypeTable): Boolean = {
    visit(assignmentStatement.expression, a)
  }

  override def visitArrayAssignmentStatement(arrayAssignmentStatement: ArrayAssignmentStatement, a: TypeTable): Boolean = {
    visit(arrayAssignmentStatement.indexExpression, a) ||
      visit(arrayAssignmentStatement.valueExpression, a)
  }


  override def visitBinaryOperationExpression(binaryOperationExpression: BinaryOperationExpression, a: TypeTable): Boolean = {
    visit(binaryOperationExpression.firstExpression, a) ||
      visit(binaryOperationExpression.secondExpression, a)
  }

  override def visitArrayAccessExpression(arrayAccessExpression: ArrayAccessExpression, a: TypeTable): Boolean = {
    visit(arrayAccessExpression.arrayExpression, a) ||
      visit(arrayAccessExpression.indexExpression, a)
  }

  override def visitArrayLengthExpression(arrayLengthExpression: ArrayLengthExpression, a: TypeTable): Boolean = {
    visit(arrayLengthExpression.arrayExpression, a)
  }

  override def visitMethodCallExpression(methodCallExpression: MethodCallExpression, a: TypeTable): Boolean = {
    methodCallExpression.
  }

  override def visitIntegerLiteral(integerLiteral: IntegerLiteral, a: TypeTable): Boolean = {
    false
  }

  override def visitTrueLiteral(a: TypeTable): Boolean = {
    false
  }

  override def visitFalseLiteral(a: TypeTable): Boolean = {
    false
  }

  override def visitIdentifierExpression(identifierExpression: IdentifierExpression, a: TypeTable): Boolean = {
    false
  }

  override def visitThisLiteral(a: TypeTable): Boolean = {
    false
  }

  override def visitNewIntArrayExpression(intArrayExpression: NewIntArrayExpression, a: TypeTable): Boolean = {
    visit(intArrayExpression.lengthExpression, a)
  }

  override def visitNewObjectExpression(newObjectExpression: NewObjectExpression, a: TypeTable): Boolean = {
    false
  }

  override def visitNegatedExpression(negatedExpression: NegatedExpression, a: TypeTable): Boolean = {
    visit(negatedExpression.expression, a)
  }

  override def visitParenedExpression(parenedExpression: ParenedExpression, a: TypeTable): Boolean = {
    visit(parenedExpression.expression, a)
  }

  private def anyContainIO(nodes: List[ASTNode], a: TypeTable): Boolean = {
    val ioChecks = for (n <- nodes)
      yield visit(n, a)

    ioChecks.contains(true)
  }
}
