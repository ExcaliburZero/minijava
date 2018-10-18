package minijava.typechecking

import minijava.grammar.{Expression, Statement}

import scala.collection.mutable

sealed trait PrimitiveType extends TypeDefinition
case object PrimitiveBooleanType extends PrimitiveType
case object PrimitiveIntType extends PrimitiveType
case object PrimitiveIntArrayType extends PrimitiveType
case object PrimitiveVoidType extends PrimitiveType

object TypeDefinition {
  // <=
  def conformsTo(left: TypeDefinition, right: TypeDefinition, typeTable: TypeTable): Option[TypeDefinition] = {
    if (left == right) {
      return Some(left)
    }

    left match {
      case FailType => return Some(FailType)
    }

    ???
  }
}

sealed trait TypeDefinition {
  def getName(): String = {
    this match {
      case ClassType(name, _, _, _) => name
      case MainClassType(name, _) => name
      case FailType => "FAIL"
      case PrimitiveIntArrayType => "int[]"
      case PrimitiveBooleanType => "boolean"
      case PrimitiveIntType => "int"
      case PrimitiveVoidType => "void"
    }
  }
}
case class ClassType(name: String, parentClass: Option[String], variables: List[Variable], methods: List[Method]) extends TypeDefinition
case class MainClassType(name: String, mainMethod: Method) extends TypeDefinition
case object FailType extends TypeDefinition

case class Variable(name: String, typeName: String)
case class Method(name: String, isIO: Boolean, returnType: String, parameters: List[Variable], localVariables: List[Variable], statements: List[Statement], returnExpression: Option[Expression])


sealed trait TypeTableError
case class TypeAlreadyExistsError(typeName: String) extends TypeTableError
case object DefineFAILClassError extends TypeTableError

class TypeTable {
  val lookupTable: mutable.HashMap[String, TypeDefinition] = mutable.HashMap()

  def add(typeName: String, typeDefinition: TypeDefinition): Either[TypeTableError, Unit] = {
    if (typeName == "FAIL") {
      return Left(DefineFAILClassError)
    }

    if (lookupTable.contains(typeName)) {
      Left(TypeAlreadyExistsError(typeName))
    } else {
      lookupTable.put(typeName, typeDefinition)
      Right()
    }
  }

  def get(typeName: String): Option[TypeDefinition] = {
    lookupTable.get(typeName)
  }

  def types(): Iterable[TypeDefinition] = {
    lookupTable.values
  }

  override def toString(): String = {
    lookupTable.keySet.mkString(", ")
  }
}
