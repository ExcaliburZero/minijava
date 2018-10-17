package minijava.typechecking

import minijava.grammar._

/*class VariableBindingVisitor(typeTable: TypeTable) extends ASTVisitor[StackedTable[String, TypeDefinition], Unit] {
  override def visitGoal(goal: Goal, a: StackedTable[String, TypeDefinition]): Unit = {
    for (r <- goal.regularClasses) visit(r, a)
  }

  override def visitRegularClass(regularClass: RegularClass, a: StackedTable[String, TypeDefinition]): Unit = {
    val classTable = a.newFrame()

    for (v <- regularClass.variableDeclarations) {
      val name = v.name.name
      val varType = v.varType match {
        case IntArrayType => PrimitiveIntArrayType
        case BooleanType => PrimitiveBooleanType
        case IntType => PrimitiveIntType
        case IdentifierType(n) => typeTable.get(n).get.typeDefinition
      }

      classTable.put(name, varType)
    }
  }
}*/
