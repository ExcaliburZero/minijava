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
    val typeDefinition = ClassType(mainClass)

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
    val typeDefinition = ClassType(regularClass)

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
}
