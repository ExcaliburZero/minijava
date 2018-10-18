package minijava.typechecking

import minijava.grammar._

/*class IOCheckingVisitor extends ASTVisitor[(TypeTable, Option[ClassType], Option[Method]), Boolean] {
  override def visitStatementBlock(statementBlock: StatementBlock, a: (TypeTable, Option[ClassType], Option[Method])): Boolean = {
    anyContainIO(statementBlock.statements, a)
  }

  override def visitIfStatement(ifStatement: IfStatement, a: (TypeTable, Option[ClassType], Option[Method])): Boolean = {
    visit(ifStatement.thenClause, a) ||
      visit(ifStatement.elseClause, a)
  }

  override def visitWhileStatement(whileStatement: WhileStatement, a: (TypeTable, Option[ClassType], Option[Method])): Boolean = {
    visit(whileStatement.statement, a)
  }

  override def visitPrintStatement(printStatement: PrintStatement, a: (TypeTable, Option[ClassType], Option[Method])): Boolean = {
    true
  }

  override def visitAssignmentStatement(assignmentStatement: AssignmentStatement, a: (TypeTable, Option[ClassType], Option[Method])): Boolean = {
    visit(assignmentStatement.expression, a)
  }

  override def visitArrayAssignmentStatement(arrayAssignmentStatement: ArrayAssignmentStatement, a: (TypeTable, Option[ClassType], Option[Method])): Boolean = {
    visit(arrayAssignmentStatement.indexExpression, a) ||
      visit(arrayAssignmentStatement.valueExpression, a)
  }


  override def visitBinaryOperationExpression(binaryOperationExpression: BinaryOperationExpression, a: (TypeTable, Option[ClassType], Option[Method])): Boolean = {
    visit(binaryOperationExpression.firstExpression, a) ||
      visit(binaryOperationExpression.secondExpression, a)
  }

  override def visitArrayAccessExpression(arrayAccessExpression: ArrayAccessExpression, a: (TypeTable, Option[ClassType], Option[Method])): Boolean = {
    visit(arrayAccessExpression.arrayExpression, a) ||
      visit(arrayAccessExpression.indexExpression, a)
  }

  override def visitArrayLengthExpression(arrayLengthExpression: ArrayLengthExpression, a: (TypeTable, Option[ClassType], Option[Method])): Boolean = {
    visit(arrayLengthExpression.arrayExpression, a)
  }

  override def visitMethodCallExpression(methodCallExpression: MethodCallExpression, a: (TypeTable, Option[ClassType], Option[Method])): Boolean = {
    ???
  }

  override def visitIntegerLiteral(integerLiteral: IntegerLiteral, a: (TypeTable, Option[ClassType], Option[Method])): Boolean = {
    false
  }

  override def visitTrueLiteral(a: (TypeTable, Option[ClassType], Option[Method])): Boolean = {
    false
  }

  override def visitFalseLiteral(a: (TypeTable, Option[ClassType], Option[Method])): Boolean = {
    false
  }

  override def visitIdentifierExpression(identifierExpression: IdentifierExpression, a: (TypeTable, Option[ClassType], Option[Method])): Boolean = {
    false
  }

  override def visitThisLiteral(a: (TypeTable, Option[ClassType], Option[Method])): Boolean = {
    false
  }

  override def visitNewIntArrayExpression(intArrayExpression: NewIntArrayExpression, a: (TypeTable, Option[ClassType], Option[Method])): Boolean = {
    visit(intArrayExpression.lengthExpression, a)
  }

  override def visitNewObjectExpression(newObjectExpression: NewObjectExpression, a: (TypeTable, Option[ClassType], Option[Method])): Boolean = {
    false
  }

  override def visitNegatedExpression(negatedExpression: NegatedExpression, a: (TypeTable, Option[ClassType], Option[Method])): Boolean = {
    visit(negatedExpression.expression, a)
  }

  override def visitParenedExpression(parenedExpression: ParenedExpression, a: (TypeTable, Option[ClassType], Option[Method])): Boolean = {
    visit(parenedExpression.expression, a)
  }

  private def anyContainIO(nodes: List[ASTNode], a: (TypeTable, Option[ClassType], Option[Method])): Boolean = {
    val ioChecks = for (n <- nodes)
      yield visit(n, a)

    ioChecks.contains(true)
  }
}*/
