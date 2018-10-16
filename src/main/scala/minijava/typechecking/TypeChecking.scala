package minijava.typechecking

import minijava.grammar.Goal
import minijava.messages.{CompilerError, CompilerMessage, TypeCheckingError}

object TypeChecking {
  def typeCheck(ast: Goal): Either[List[CompilerMessage], TypeTable] = {
    val (typeTable, duplicateTypeErrors) = extractTypeTable(ast)

    if (duplicateTypeErrors.nonEmpty) {
      return Left(duplicateTypeErrors)
    }

    println(typeTable)

    ???
  }

  def extractTypeTable(ast: Goal): (TypeTable, List[CompilerMessage]) = {
    val typeExtractionVisitor = new TypeExtractionVisitor()

    typeExtractionVisitor.visitGoal(ast, ())
    typeExtractionVisitor.getTypeTable()
  }
}
