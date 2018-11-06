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

    if (left == FailType || right == FailType) {
      return Some(FailType)
    }

    if (left.isInstanceOf[PrimitiveType] || right.isInstanceOf[PrimitiveType]) {
      return None
    }

    left match {
      case leftClass: ClassLikeType =>
        leftClass.getParentClass() match {
          case Some(p) =>
            val leftParent = typeTable.get(p).get
            return conformsTo(leftParent, right, typeTable)
          case None =>
        }
      case _ =>
    }

    None
  }
}

sealed trait TypeDefinition {
  def getName(): String = {
    this match {
      case ClassType(name, _, _, _) => name
      case MainClassType(name, _) => name
      case FailType => "*FAIL"
      case PrimitiveIntArrayType => "int[]"
      case PrimitiveBooleanType => "boolean"
      case PrimitiveIntType => "int"
      case PrimitiveVoidType => "void"
    }
  }
}
case object FailType extends TypeDefinition

sealed trait VariableContext
case class MethodVariable(method: Method, location: MethodVariableLocation) extends VariableContext

sealed trait MethodVariableLocation
case object LocalVariable extends MethodVariableLocation
case object Parameter extends MethodVariableLocation

sealed trait ClassLikeType extends TypeDefinition with VariableContext {
  def getParentClass(): Option[String] = {
    this match {
      case classType: ClassType => classType.parentClass
      case _: MainClassType => None
    }
  }

  def getVariables(): List[Variable] = {
    this match {
      case classType: ClassType => classType.variables
      case _: MainClassType => List()
    }
  }

  def getMethods(): List[Method] = {
    this match {
      case classType: ClassType => classType.methods
      case _: MainClassType => List()
    }
  }
}
case class ClassType(name: String, parentClass: Option[String], variables: List[Variable], methods: List[Method]) extends ClassLikeType
case class MainClassType(name: String, mainMethod: Method) extends ClassLikeType

case class Variable(name: String, typeName: String)
case class Method(name: String, isIO: Boolean, returnType: String, parameters: List[Variable], localVariables: List[Variable], statements: List[Statement], returnExpression: Option[Expression]) {
  def getSignature(): String = {
    val paramStrings = parameters.map(_.typeName).map(convertType)
    val returnString = convertType(returnType)

    "(" + paramStrings.mkString("") + ")" + returnString
  }

  private def convertType(t: String): String = {
    t match {
      case "int" => "I"
    }
  }
}


sealed trait TypeTableError
case class TypeAlreadyExistsError(typeName: String) extends TypeTableError

class TypeTable {
  val lookupTable: mutable.HashMap[String, TypeDefinition] = mutable.HashMap()

  def add(typeName: String, typeDefinition: TypeDefinition): Either[TypeTableError, Unit] = {
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

  def types(): List[TypeDefinition] = {
    lookupTable.values.toList
  }

  override def toString(): String = {
    lookupTable.keySet.mkString(", ")
  }
}
