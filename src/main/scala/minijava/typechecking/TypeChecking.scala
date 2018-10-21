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

    val duplicateMethodErrors = checkForDuplicateMethods(typeTable)

    if (duplicateMethodErrors.nonEmpty) {
      return Left(duplicateMethodErrors)
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

  def checkForDuplicateMethods(typeTable: TypeTable): List[CompilerMessage] = {
    val errors = new ArrayBuffer[CompilerMessage]()

    for (t <- typeTable.types()) {
      t match {
        case classType: ClassType =>
          // A method is a duplicate if there is at least one other method declaration with the same name, parameters,
          // and return type in the same class
          classType.methods.groupBy(m => (m.name, m.parameters, m.returnType))
            .filter(_._2.length > 1)
            .foreach(dm => {
              failDuplicateMethod(classType.getName(), dm._1, dm._2.length, errors)
            })
        case _ =>
      }
    }

    errors.toList
  }

  def failDuplicateMethod(className: String, methodInfo: (String, List[Variable], String), numDuplicates: Int, errors: ArrayBuffer[CompilerMessage]): Unit = {
    val methodName = methodInfo._1
    val params = methodInfo._2
    val returnType = methodInfo._3

    val message = "Duplicate declaration(s) of method \"%s %s(%s)\" for class \"%s\"."
      .format(
        returnType, methodName,
        params.map(_.typeName).mkString(", "),
        className
      )

    val err = CompilerMessage(CompilerError, TypeCheckingError, None, message)

    errors.append(err)
  }

  def validateMethodOverloading(typeTable: TypeTable): List[CompilerMessage] = {
    val errors = new ArrayBuffer[CompilerMessage]()

    for (t <- typeTable.types()) {
      t match {
        case classType: ClassType =>

          val classMethods = getAllClassMethods(classType, typeTable)

          classMethods.groupBy(_._1.name)
            .foreach(nm => {
              val name = nm._1
              val methodsSameName = nm._2

              methodsSameName.groupBy(_._1.parameters.map(_.typeName))
                .foreach(pm => {
                  val parameterTypes = pm._1
                  val methodMatches = pm._2

                  val childMatches = methodMatches.filter(_._2 == false).map(_._1)
                  val parentMatches = methodMatches.filter(_._2 == true).map(_._1)

                  // Filter out the parent methods that have return types that are conformed to by the return types of
                  // child class methods
                  val parentNonCovariant = parentMatches.filter(p => {
                    childMatches.exists(c => TypeDefinition.conformsTo(
                      typeTable.get(c.returnType).get,
                      typeTable.get(p.returnType).get,
                      typeTable
                    ).isEmpty)
                  })

                  if ((childMatches ++ parentNonCovariant).distinct.length > 1) {
                    failBadMethodOverloading(classType.getName(), name, parameterTypes, childMatches ++ parentNonCovariant, errors)
                  }
                })
            })
        case _ =>
      }
    }

    errors.toList
  }

  def getAllClassMethods(classType: ClassLikeType, typeTable: TypeTable): List[(Method, Boolean)] = {
    classType.getParentClass() match {
      case None => classType.getMethods().map((_, false))
      case Some(p) => classType.getMethods().map((_, false)) ++ getAllClassMethods(typeTable.get(p).get.asInstanceOf[ClassLikeType], typeTable).map(t => (t._1, true))
    }
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
