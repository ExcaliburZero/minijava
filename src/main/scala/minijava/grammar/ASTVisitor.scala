package minijava.grammar

abstract class ASTVisitor[A, B] {
  def visit(node: ASTNode, a: A): B = {
    node match {
      case n: Goal => visitGoal(n, a)
      case n: MainClass => visitMainClass(n, a)
      case n: ClassDeclaration => visitClassDeclaration(n, a)
    }
  }

  def visitGoal(goal: Goal, a: A): B = ???
  def visitMainClass(mainClass: MainClass, a: A): B = ???
  def visitClassDeclaration(classDeclaration: ClassDeclaration, a: A): B = ???
}