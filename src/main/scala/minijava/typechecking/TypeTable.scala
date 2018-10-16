package minijava.typechecking

import minijava.grammar.ClassDeclaration

import scala.collection.mutable

sealed trait PrimitiveType extends TypeDefinition
case object PrimitiveBooleanType extends PrimitiveType
case object PrimitiveIntType extends PrimitiveType
case object PrimitiveIntArrayType extends PrimitiveType

sealed trait TypeDefinition
//case class ArrayType(elementType: TypeDefinition) extends TypeDefinition
case class ClassType(classDeclaration: ClassDeclaration) extends TypeDefinition
case object FailType extends TypeDefinition

case class TypeTableEntry(typeName: String, typeDefinition: TypeDefinition) {
  def isPrimitive(): Boolean = {
    typeDefinition match {
      case _: PrimitiveType => true
      case _ => false
    }
  }
}

sealed trait TypeTableError
case class TypeAlreadyExistsError(typeName: String) extends TypeTableError
case object DefineFAILClassError extends TypeTableError

class TypeTable {
  val lookupTable: mutable.HashMap[String, TypeTableEntry] = mutable.HashMap()

  def add(typeName: String, typeDefinition: TypeDefinition): Either[TypeTableError, Unit] = {
    if (typeName == "FAIL") {
      return Left(DefineFAILClassError)
    }

    if (lookupTable.contains(typeName)) {
      Left(TypeAlreadyExistsError(typeName))
    } else {
      val entry = TypeTableEntry(typeName, typeDefinition)

      lookupTable.put(typeName, entry)
      Right()
    }
  }

  def get(typeName: String): Option[TypeTableEntry] = {
    lookupTable.get(typeName)
  }

  override def toString(): String = {
    lookupTable.keySet.mkString(", ")
  }
}
