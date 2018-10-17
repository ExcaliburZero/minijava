package minijava.grammar

abstract class ASTVisitor[A, B] {
  def visit(node: ASTNode, a: A): B = {
    node match {
      case n: Goal => visitGoal(n, a)
      case n: MainClass => visitMainClass(n, a)
      case n: RegularClass => visitRegularClass(n, a)
    }
  }

  def visitGoal(goal: Goal, a: A): B = ???
  def visitMainClass(mainClass: MainClass, a: A): B = ???
  def visitRegularClass(regularClass: RegularClass, a: A): B = ???

  def visitStatementBlock(statementBlock: StatementBlock, a: A): B = ???
  def visitIfStatement(ifStatement: IfStatement, a: A): B = ???
  def visitWhileStatement(whileStatement: WhileStatement, a: A): B = ???
  def visitPrintStatement(printStatement: PrintStatement, a: A): B = ???
  def visitAssignmentStatement(assignmentStatement: AssignmentStatement, a: A): B = ???
  def visitArrayAssignmentStatement(arrayAssignmentStatement: ArrayAssignmentStatement, a: A): B = ???

  def visitBinaryOperationExpression(binaryOperationExpression: BinaryOperationExpression, a: A): B = ???
  def visitArrayAccessExpression(arrayAccessExpression: ArrayAccessExpression, a: A): B = ???
  def visitArrayLengthExpression(arrayLengthExpression: ArrayLengthExpression, a: A): B = ???
  def visitMethodCallExpression(methodCallExpression: MethodCallExpression, a: A): B = ???
  def visitIntegerLiteral(integerLiteral: IntegerLiteral, a: A): B = ???
  def visitTrueLiteral(a: A): B = ???
  def visitFalseLiteral(a: A): B = ???
  def visitIdentifierExpression(identifierExpression: IdentifierExpression, a: A): B = ???
  def visitThisLiteral(a: A): B = ???
  def visitNewIntArrayExpression(intArrayExpression: NewIntArrayExpression, a: A): B = ???
  def visitNewObjectExpression(newObjectExpression: NewObjectExpression, a: A): B = ???
  def visitNegatedExpression(negatedExpression: NegatedExpression, a: A): B = ???
  def visitParenedExpression(parenedExpression: ParenedExpression, a: A): B = ???
}
