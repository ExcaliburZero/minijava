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
    (typeTable, typeCheckingErrors.toList)
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

  private def typeToName(t: Type): String = {
    t match {
      case IntArrayType => "int[]"
      case BooleanType => "boolean"
      case IntType => "int"
      case IdentifierType(n) => n.name
    }
  }
}
