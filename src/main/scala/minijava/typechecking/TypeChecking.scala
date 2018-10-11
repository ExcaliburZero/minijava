package minijava.typechecking

import minijava.grammar.Goal
import minijava.messages.{CompilerError, CompilerMessage, TypeCheckingError}

object TypeChecking {
  def typeCheck(ast: Goal): Either[List[CompilerMessage], TypeTable] = {
    val (typeTable, duplicateTypeErrors) = extractTypeTable(ast)

    if (duplicateTypeErrors.nonEmpty) {
      return Left(duplicateTypeErrors.map(e => {
        val message = "Duplicate declaration of class \"%s\"".format(e.typeName)

        CompilerMessage(CompilerError, TypeCheckingError, None, message)
      }))
    }

    println(typeTable)

    ???
  }

  def extractTypeTable(ast: Goal): (TypeTable, List[TypeAlreadyExistsError]) = {
    val typeExtractionVisitor = new TypeExtractionVisitor()

    typeExtractionVisitor.visitGoal(ast, ())
    typeExtractionVisitor.getTypeTable()
  }
}
