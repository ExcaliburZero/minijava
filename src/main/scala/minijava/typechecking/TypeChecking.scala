package minijava.typechecking

import minijava.grammar.Goal
import minijava.messages.{CompilerError, CompilerMessage, TypeCheckingError}

import scala.collection.mutable.ArrayBuffer

object TypeChecking {
  def typeCheck(ast: Goal): Either[List[CompilerMessage], TypeTable] = {
    val (typeTable, duplicateTypeErrors) = extractTypeTable(ast)

    if (duplicateTypeErrors.nonEmpty) {
      return Left(duplicateTypeErrors)
    }

    val overloadingErrors = validateMethodOverloading(typeTable)

    if (overloadingErrors.nonEmpty) {
      return Left(overloadingErrors)
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

  def validateMethodOverloading(typeTable: TypeTable): List[CompilerMessage] = {
    val errors = new ArrayBuffer[CompilerMessage]()

    for (t <- typeTable.types()) {
      t match {
        case classType: ClassType =>
          classType.methods.groupBy(_.name)
            .foreach(nm => {
              val name = nm._1
              val methodsSameName = nm._2

              methodsSameName.groupBy(_.parameters.map(_.typeName))
                .foreach(pm => {
                  val parameterTypes = pm._1
                  val methodMatches = pm._2

                  if (methodMatches.length > 1) {
                    failBadMethodOverloading(classType.getName(), name, parameterTypes, methodMatches, errors)
                  }
                })
            })
        case _ =>
      }
    }

    errors.toList
  }

  def failBadMethodOverloading(className: String, methodName: String, parameterTypes: List[String], matches: List[Method], errors: ArrayBuffer[CompilerMessage]): Unit = {
    val parameters = parameterTypes.mkString(", ")

    val messageStart = "Multiple methods \"%s(%s)\" exist for class \"%s\":\n\n".format(methodName, parameters, className)

    val versions = for (m <- matches.sortBy(_.returnType))
      yield "%s %s(%s)".format(m.returnType, m.name, m.parameters.map(_.typeName).mkString(","))

    val messageEnd = "\n\nNote that overloading based on return type is not allowed."

    val message = messageStart + versions.mkString("\n") + messageEnd

    val err = CompilerMessage(CompilerError, TypeCheckingError, None, message)

    errors.append(err)
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
