package minijava.typechecking

import minijava.grammar._
import minijava.messages.{CompilerError, CompilerMessage, LineNumber, TypeCheckingError}

import scala.collection.mutable.ArrayBuffer

object TypeExtractionVisitor {
  def createTypeTable(): TypeTable = {
    val typeTable = new TypeTable()

    typeTable.add("int", PrimitiveIntType)
    typeTable.add("int[]", PrimitiveIntArrayType)
    typeTable.add("boolean", PrimitiveBooleanType)
    typeTable.add("void", PrimitiveVoidType)
    typeTable.add("FAIL", FailType)

    typeTable
  }
}

class TypeExtractionVisitor extends ASTVisitor[Unit, Unit] {
  private val typeTable = TypeExtractionVisitor.createTypeTable()
  private val typeCheckingErrors = new ArrayBuffer[CompilerMessage]()

  def getTypeTable(): (TypeTable, List[CompilerMessage]) = {
    for (t <- typeTable.types().sortBy(_.getName())) {
      checkForCircularInheritance(t.getName(), t, List(), typeTable)
    }

    (typeTable, typeCheckingErrors.toList)
  }

  private def checkForCircularInheritance(startClass: String, t: TypeDefinition, seen: List[String], typeTable: TypeTable): Unit = {
    t match {
      case classLikeType: ClassLikeType =>
        val newSeen = t.getName() :: seen

        classLikeType.getParentClass() match {
          case None =>
          case Some(parent) =>
            if (newSeen.contains(parent)) {
              val message = "Circular inheritance found for class %s, repeating at class %s".format(startClass, parent)

              val compilerMessage = CompilerMessage(CompilerError, TypeCheckingError, None,
                message)

              typeCheckingErrors.append(compilerMessage)
            } else {
              typeTable.get(parent) match {
                case Some(p) => checkForCircularInheritance(startClass, p, newSeen, typeTable)
                case None => if (startClass == t.getName())
                  failParentClassDoesNotExist(t.getName(), parent)
              }
            }
        }
      case _ =>
    }
  }

  override def visitGoal(goal: Goal, a: Unit): Unit = {
    val mainClass = goal.mainClass
    val regularClasses = goal.regularClasses

    visit(mainClass, a)
    for (cd <- regularClasses) visit(cd, a)
  }

  override def visitMainClass(mainClass: MainClass, a: Unit): Unit = {
    val className = mainClass.name.name

    val mainMethod = Method("main", mainClass.isIO, "void", List(), List(), List(mainClass.statement), None)
    val typeDefinition = MainClassType(className, mainMethod)

    typeTable.add(className, typeDefinition)
      .left.map {
        case TypeAlreadyExistsError(name) =>
          addDuplicateClassError(name, mainClass.line)
        case DefineFAILClassError =>
          addDefineFAILClassError(mainClass.line)
      }
  }

  override def visitRegularClass(regularClass: RegularClass, a: Unit): Unit = {
    val className = regularClass.name.name
    val parentClass = regularClass.parentClass.map(_.name)

    checkForDuplicateClassVars(regularClass.variableDeclarations, className)

    val variables = regularClass.variableDeclarations.map(vd => {
      val t = typeToName(vd.varType)
      if (t == "FAIL") {
        addFAILVariableError(vd.line, vd.name.name, "class instance variable")
      }

      Variable(vd.name.name, t)
    })
    val methods = regularClass.methodDeclarations.map(md => {
      val name = md.name.name
      val isIO = md.isIO
      val returnType = typeToName(md.varType)
      val parameters = md.parameters.map(p => {
        if (typeToName(p._1) == "FAIL") {
          addFAILVariableError(md.line, p._2.name, "parameter")
        }

        Variable(p._2.name, typeToName(p._1))
      })
      val localVariables = md.variableDeclarations.map(p => {
        if (typeToName(p.varType) == "FAIL") {
          addFAILVariableError(p.line, p.name.name, "local variable")
        }

        Variable(p.name.name, typeToName(p.varType))
      })
      val statements = md.statements
      val returnExpression = Some(md.returnExpression)

      checkForDuplicateMethodVars(parameters,
        localVariables.zip(md.variableDeclarations.map(_.line)),
        className, md.name.name)

      Method(name, isIO, returnType, parameters, localVariables, statements, returnExpression)
    })

    val typeDefinition = ClassType(className, parentClass, variables, methods)

    typeTable.add(className, typeDefinition)
      .left.map {
        case TypeAlreadyExistsError(name) =>
          addDuplicateClassError(name, regularClass.line)
        case DefineFAILClassError =>
          addDefineFAILClassError(regularClass.line)
      }
  }

  private def checkForDuplicateClassVars(variableDeclarations: List[VariableDeclaration], className: String): Unit = {
    val variableNames = variableDeclarations.map(_.name.name)

    for (v <- variableNames.distinct) {
      if (variableNames.count(_ == v) > 1) {
        val message = "Duplicate declaration of class variable \"%s\" of class %s".format(v, className)

        val lines = variableDeclarations
          .filter(_.name.name == v)
          .map(_.line)
          .drop(1)

        for (line <- lines) {
          val compilerMessage = CompilerMessage(CompilerError, TypeCheckingError, Some(LineNumber(line)),
            message)

          typeCheckingErrors.append(compilerMessage)
        }
      }
    }
  }

  private def checkForDuplicateMethodVars(parameters: List[Variable], localVariablesAndLine: List[(Variable, Int)], className: String, methodName: String): Unit = {
    val localVariables = localVariablesAndLine.map(_._1)

    val localNames = localVariables.map(_.name)
    val paramNames = parameters.map(_.name)

    val allVars = paramNames ++ localNames

    for (v <- allVars.distinct) {
      if (allVars.count(_ == v) > 1) {
        if (paramNames.contains(v)) {
          val message = "Duplicate declaration of parameter \"%s\" in method %s of class %s".format(v, methodName, className)

          // Multiple parameters with same name
          for (_ <- 1 until paramNames.count(_ == v)) {
            val compilerMessage = CompilerMessage(CompilerError, TypeCheckingError, None,
              message)

            typeCheckingErrors.append(compilerMessage)
          }

          // Local re-declarations
          val localLines = localVariablesAndLine
            .filter(_._1.name == v)
            .map(_._2)

          for (line <- localLines) {
            val compilerMessage = CompilerMessage(CompilerError, TypeCheckingError, Some(LineNumber(line)),
              message)

            typeCheckingErrors.append(compilerMessage)
          }
        } else {
          val message = "Duplicate declaration of local variable \"%s\" in method %s of class %s".format(v, methodName, className)

          val lines = localVariablesAndLine
            .filter(_._1.name == v)
            .map(_._2)
            .drop(1)

          for (line <- lines) {
            val compilerMessage = CompilerMessage(CompilerError, TypeCheckingError, Some(LineNumber(line)),
              message)

            typeCheckingErrors.append(compilerMessage)
          }
        }
      }
    }
  }

  private def addDuplicateClassError(className: String, lineNumber: Int): Unit = {
    val message = "Duplicate declaration of class \"%s\"".format(className)

    val compilerMessage = CompilerMessage(CompilerError, TypeCheckingError, Some(LineNumber(lineNumber)),
      message)

    typeCheckingErrors.append(compilerMessage)
  }

  private def addDefineFAILClassError(lineNumber: Int): Unit = {
    val message = "Attempt to create class with reserved name \"FAIL\""

    val compilerMessage = CompilerMessage(CompilerError, TypeCheckingError, Some(LineNumber(lineNumber)),
      message)

    typeCheckingErrors.append(compilerMessage)
  }

  private def addFAILVariableError(lineNumber: Int, variableName: String, context: String): Unit = {
    val message = "Attempt to create a %s \"%s\" with reserved type \"FAIL\"".format(context, variableName)

    val compilerMessage = CompilerMessage(CompilerError, TypeCheckingError, Some(LineNumber(lineNumber)),
      message)

    typeCheckingErrors.append(compilerMessage)
  }

  private def failParentClassDoesNotExist(className: String, parentName: String): Unit = {
    val message = "Class \"%s\" extends unknown class \"%s\".".format(className, parentName)

    val compilerMessage = CompilerMessage(CompilerError, TypeCheckingError, None,
      message)

    typeCheckingErrors.append(compilerMessage)
  }

  private def typeToName(t: Type): String = {
    t match {
      case IntArrayType => "int[]"
      case BooleanType => "boolean"
      case IntType => "int"
      case IdentifierType(n) => n.name
    }
  }
}
