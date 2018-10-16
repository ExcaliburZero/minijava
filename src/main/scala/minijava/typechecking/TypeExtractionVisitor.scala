package minijava.typechecking

import minijava.grammar._
import minijava.messages.{CompilerError, CompilerMessage, LineNumber, TypeCheckingError}

import scala.collection.mutable.ArrayBuffer

object TypeExtractionVisitor {
  /*def mainClassToRegularClass(mainClass: MainClass): ClassDeclaration = {
    mainClass match {
      case MainClass(name, isIO, parameter, statement, line) =>
        val variables = List()
        val methods = List(
          MethodDeclaration(
            isIO, IdentifierType(Identifier("Any")), Identifier("main"),
            List(), List(), List(statement),
          )
        )

        ClassDeclaration(name, None, variables, methods, line)
    }
  }*/
}

class TypeExtractionVisitor extends ASTVisitor[Unit, Unit] {
  private val typeTable = new TypeTable()
  private val duplicateTypeErrors = new ArrayBuffer[CompilerMessage]()

  def getTypeTable(): (TypeTable, List[CompilerMessage]) = {
    (typeTable, duplicateTypeErrors.toList)
  }

  override def visitGoal(goal: Goal, a: Unit): Unit = {
    val mainClass = goal.mainClass
    val classDeclarations = goal.classDeclarations

    //visit(mainClass, a)
    for (cd <- classDeclarations) visit(cd, a)
  }

  /*override def visitMainClass(mainClass: MainClass, a: Unit): Unit = {
    val className = mainClass.name.name
    val typeDefinition = ClassType(mainClass)

    typeTable.add(className, typeDefinition)
      .left.map(_ => {
      val message = "Duplicate declaration of main class \"%s\"".format(className)

      val compilerMessage = CompilerMessage(CompilerError, TypeCheckingError, Some(LineNumber(mainClass.line)),
        message)

      duplicateTypeErrors.append(compilerMessage)
    })
  }*/

  override def visitClassDeclaration(classDeclaration: ClassDeclaration, a: Unit): Unit = {
    val className = classDeclaration.name.name
    val typeDefinition = ClassType(classDeclaration)

    typeTable.add(className, typeDefinition)
      .left.map(_ => {
        val message = "Duplicate declaration of class \"%s\"".format(className)

        val compilerMessage = CompilerMessage(CompilerError, TypeCheckingError, Some(LineNumber(classDeclaration.line)),
          message)

        duplicateTypeErrors.append(compilerMessage)
      })
  }
}
