package minijava.grammar

abstract class ASTVisitor[A, B] {
  def visitGoal(goal: Goal, a: A): B = ???
  def visitMainClass(mainClass: MainClass, a: A): B = ???
  def visitClassDeclaration(classDeclaration: ClassDeclaration, a: A): B = ???
}
