package minijava.typechecking

import minijava.grammar.{ASTVisitor, ClassDeclaration, Goal}

import scala.collection.mutable.ArrayBuffer

class TypeExtractionVisitor extends ASTVisitor[Unit, Unit] {
  private val typeTable = new TypeTable()
  private val duplicateTypeErrors = new ArrayBuffer[TypeAlreadyExistsError]()

  def getTypeTable(): (TypeTable, List[TypeAlreadyExistsError]) = {
    (typeTable, duplicateTypeErrors.toList)
  }

  override def visitGoal(goal: Goal, a: Unit): Unit = {
    val mainClass = goal.mainClass
    val classDeclarations = goal.classDeclarations

    // TODO: visitMainClass ?

    for (cd <- classDeclarations) visitClassDeclaration(cd, a)
  }

  override def visitClassDeclaration(classDeclaration: ClassDeclaration, a: Unit): Unit = {
    val className = classDeclaration.name.name
    val typeDefinition = ClassType(classDeclaration)

    typeTable.add(className, typeDefinition)
      .left.map(duplicateTypeErrors.append(_))
  }
}
