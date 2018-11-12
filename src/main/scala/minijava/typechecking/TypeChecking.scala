package minijava.typechecking

import minijava.grammar.{Goal, MethodCallExpression, PrintStatement}
import minijava.messages._

import scala.collection.mutable.ArrayBuffer

object TypeChecking {
  def typeCheck(ast: Goal): (Option[TypeTable], Option[List[CompilerMessage]]) = {
    val (typeTable, duplicateTypeErrors) = extractTypeTable(ast)

    if (duplicateTypeErrors.nonEmpty) {
      return (None, Some(duplicateTypeErrors))
    }

    val unknownTypeErrors = checkForUnknownTypes(typeTable)
    val duplicateMethodErrors = checkForDuplicateMethods(typeTable)

    val stage2Errors = unknownTypeErrors ++ duplicateMethodErrors
    if (stage2Errors.nonEmpty) {
      return (None, Some(stage2Errors))
    }

    val overloadingErrors = validateMethodOverloading(typeTable)

    if (overloadingErrors.nonEmpty) {
      return (None, Some(overloadingErrors))
    }

    val typeCheckingErrors = visitingTypeCheck(typeTable)

    if (typeCheckingErrors.nonEmpty) {
      return (Some(typeTable), Some(typeCheckingErrors))
    }

    val ioCheckingErrors = visitingIOCheck(typeTable)

    if (ioCheckingErrors.nonEmpty) {
      return (Some(typeTable), Some(ioCheckingErrors))
    }

    (Some(typeTable), None)
  }

  private def extractTypeTable(ast: Goal): (TypeTable, List[CompilerMessage]) = {
    val typeExtractionVisitor = new TypeExtractionVisitor()

    typeExtractionVisitor.visitGoal(ast, ())
    typeExtractionVisitor.getTypeTable()
  }

  private def checkForDuplicateMethods(typeTable: TypeTable): List[CompilerMessage] = {
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

  private def failDuplicateMethod(className: String, methodInfo: (String, List[Variable], String), numDuplicates: Int, errors: ArrayBuffer[CompilerMessage]): Unit = {
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

  private def validateMethodOverloading(typeTable: TypeTable): List[CompilerMessage] = {
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

                  val childIO = childMatches.filter(_.isIO)
                  val parentIO = parentMatches.filter(_.isIO)
                  if (childIO.nonEmpty && parentIO.isEmpty && parentMatches.nonEmpty) {
                    failIOOverride(classType, childIO, errors)
                  }
                })
            })
        case _ =>
      }
    }

    errors.toList
  }

  private def getAllClassMethods(classType: ClassLikeType, typeTable: TypeTable): List[(Method, Boolean)] = {
    classType.getParentClass() match {
      case None => classType.getMethods().map((_, false))
      case Some(p) => classType.getMethods().map((_, false)) ++ getAllClassMethods(typeTable.get(p).get.asInstanceOf[ClassLikeType], typeTable).map(t => (t._1, true))
    }
  }

  private def failBadMethodOverloading(className: String, methodName: String, parameterTypes: List[String], matches: List[Method], errors: ArrayBuffer[CompilerMessage]): Unit = {
    val parameters = parameterTypes.mkString(", ")

    val messageStart = "Multiple methods \"%s(%s)\" exist for class \"%s\":\n\n".format(methodName, parameters, className)

    val versions = for (m <- matches.sortBy(_.returnType))
      yield "%s %s(%s)".format(m.returnType, m.name, m.parameters.map(_.typeName).mkString(","))

    val messageEnd = "\n\nNote that overloading based on return type is not allowed."

    val message = messageStart + versions.mkString("\n") + messageEnd

    val err = CompilerMessage(CompilerError, TypeCheckingError, None, message)

    errors.append(err)
  }

  private def visitingTypeCheck(typeTable: TypeTable): List[CompilerMessage] = {
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

  private def typeCheckMethod(visitor: TypeCheckingVisitor, typeTable: TypeTable, classType: ClassLikeType, method: Method): Unit = {
    val context = TypeVisitorContext(typeTable, classType, method)

    for (s <- method.statements) {
      visitor.visit(s, context)
    }

    classType match {
      case _: MainClassType =>
      case _ =>
        visitor.checkReturnType(
          typeTable.get(method.returnType) match {
            case Some(c) => c
            case None => FailType
          },
          method.returnExpression.get,
          context
        )
    }
  }

  private def checkForUnknownTypes(typeTable: TypeTable): List[CompilerMessage] = {
    val errors = new ArrayBuffer[CompilerMessage]()

    for (t <- typeTable.types()) {
      t match {
        case classType: ClassType =>
          classType.methods.foreach(m => {
            for (v <- m.parameters) {
              if (typeTable.get(v.typeName).isEmpty) {
                failUnknownVariableType(v, "parameter", m, classType, errors)
              }
            }

            for (v <- m.localVariables) {
              if (typeTable.get(v.typeName).isEmpty) {
                failUnknownVariableType(v, "local variable", m, classType, errors)
              }
            }
          })

          for (v <- classType.variables) {
            if (typeTable.get(v.typeName).isEmpty) {
              failUnknownClassVariableType(v, classType, errors)
            }
          }
        case _ =>
      }
    }

    errors.toList
  }

  private def ioCheckMethod(typeTable: TypeTable, classType: ClassLikeType, method: Method, errors: ArrayBuffer[CompilerMessage]): Unit = {
    val shouldBeIO = method.isIO

    val visitor = new IOCheckingVisitor()

    method.statements.foreach(visitor.visit(_, ()))
    method.returnExpression.foreach(visitor.visit(_, ()))

    val ioInstances = visitor.getIOInstances()

    (shouldBeIO, ioInstances.nonEmpty) match {
      case (true, true) => ()
      case (false, false) => ()
      case (true, false) => warnUnnecessaryIOMark(classType, method, errors)
      case (false, true) => failUnmarkedIO(classType, method, ioInstances, errors)
    }
  }

  private def visitingIOCheck(typeTable: TypeTable): List[CompilerMessage] = {
    val errors = new ArrayBuffer[CompilerMessage]()

    for (t <- typeTable.types()) {
      t match {
        case classType: ClassType =>
          for (m <- classType.methods) {
            ioCheckMethod(typeTable, classType, m, errors)
          }
        case mainClassType: MainClassType =>
          ioCheckMethod(typeTable, mainClassType, mainClassType.mainMethod, errors)
        case _ =>
      }
    }

    errors.toList
  }

  private def failUnknownVariableType(variable: Variable, context: String, method: Method, classLikeType: ClassLikeType, errors: ArrayBuffer[CompilerMessage]): Unit = {
    val message = "Unknown type \"%s\" for %s \"%s\" in method \"%s\" of class \"%s\"."
      .format(variable.typeName, context, variable.name, method.name, classLikeType.getName())

    val err = CompilerMessage(CompilerError, TypeCheckingError, None, message)

    errors.append(err)
  }

  private def failUnknownClassVariableType(variable: Variable, classLikeType: ClassLikeType, errors: ArrayBuffer[CompilerMessage]): Unit = {
    val message = "Unknown type \"%s\" for class variable \"%s\" in class \"%s\"."
      .format(variable.typeName, variable.name, classLikeType.getName())

    val err = CompilerMessage(CompilerError, TypeCheckingError, None, message)

    errors.append(err)
  }

  private def failIOOverride(classLikeType: ClassLikeType, childIOMethods: List[Method], errors: ArrayBuffer[CompilerMessage]): Unit = {
    val message = "IO method \"%s\" of child class \"%s\" overrides non-io method in parent class \"%s\"."
      .format(childIOMethods.head.name, classLikeType.getName(), classLikeType.getParentClass().get)

    val err = CompilerMessage(CompilerError, TypeCheckingError, None, message)

    errors.append(err)
  }

  private def failUnmarkedIO(classLikeType: ClassLikeType, method: Method, ioInstances: List[IOInstance], errors: ArrayBuffer[CompilerMessage]): Unit = {
    val messageStart = "IO present in non-io method \"%s\" of class \"%s\"."
      .format(method.name, classLikeType.getName())

    val messageEnd = "\n\n" + ioInstances.map(ioInst => {
      val loc = ioInst.location match {
        case LineNumber(l) => "ln:%s\t".format(l)
        case LineColumn(l, c) => "ln:%s col:%s\t".format(l, c)
      }

      val nodeType = ioInst.node match {
        case _: PrintStatement => "Print statement"
        case methodCallExp: MethodCallExpression => "Call of io method \"%s\""
            .format(methodCallExp.methodName.name)
        case _ => ???
      }

      loc + nodeType
    }).mkString("\n")

    val message = messageStart + messageEnd

    val err = CompilerMessage(CompilerError, TypeCheckingError, None, message)

    errors.append(err)
  }

  private def warnUnnecessaryIOMark(classType: ClassLikeType, method: Method, errors: ArrayBuffer[CompilerMessage]): Unit = {
    val message = "Method \"%s\" of class \"%s\" is marked as io, but does not perform any io."
      .format(method.name, classType.getName())

    val err = CompilerMessage(CompilerWarning, TypeCheckingError, None, message)

    errors.append(err)
  }
}
