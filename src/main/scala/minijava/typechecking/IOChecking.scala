package minijava.typechecking

object IOChecking {
  def doesMethodDoIO(typeTable: TypeTable, method: Method): Boolean = {
    val visitor = new IOCheckingVisitor()

    val checkIO = for (s <- method.statements)
      yield visitor.visit(s, typeTable)

    val returnIsIO = method.returnExpression.exists(exp => visitor.visit(exp, typeTable))

    checkIO.contains(true) || returnIsIO
  }
}
