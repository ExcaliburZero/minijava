package minijava.typechecking

import minijava.grammar.Goal
import minijava.messages.{CompilerError, CompilerMessage, TypeCheckingError}

object TypeChecking {
  def typeCheck(ast: Goal): Either[List[CompilerMessage], TypeTable] = {
    val (typeTable, duplicateTypeErrors) = extractTypeTable(ast)

    if (duplicateTypeErrors.nonEmpty) {
      return Left(duplicateTypeErrors)
    }

    val typeCheckingErrors = visitingTypeCheck(typeTable)

    if (typeCheckingErrors.nonEmpty) {
      return Left(typeCheckingErrors)
    }

    Right(typeTable)
  }

  def extractTypeTable(ast: Goal): (TypeTable, List[CompilerMessage]) = {
    val typeExtractionVisitor = new TypeExtractionVisitor()

    typeExtractionVisitor.visitGoal(ast, ())
    typeExtractionVisitor.getTypeTable()
  }

  def visitingTypeCheck(typeTable: TypeTable): List[CompilerMessage] = {
    val visitor = new TypeCheckingVisitor()

    for (t <- typeTable.types()) {
      t match {
        case classType: ClassType =>
          for (m <- classType.methods) {
            typeCheckMethod(visitor, typeTable, classType, m)
          }
        case mainClassType: MainClassType =>
          typeCheckMethod(visitor, typeTable, mainClassType, mainClassType.mainMethod)
        case _ =>
      }
    }

    visitor.getTypeCheckingErrors()
  }

  def typeCheckMethod(visitor: TypeCheckingVisitor, typeTable: TypeTable, classType: ClassLikeType, method: Method): Unit = {
    val context = TypeVisitorContext(typeTable, classType, method)

    for (s <- method.statements) {
      visitor.visit(s, context)
    }

    method.returnExpression.foreach(e => visitor.visit(e, context))
  }
}
