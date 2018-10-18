package minijava.typechecking

import minijava.grammar._

case class BinaryOperationDefinition(node: BinaryOperator, firstParamType: TypeDefinition, secondParamType: TypeDefinition, returnType: TypeDefinition)
case class UnaryOperationDefinition(paramType: TypeDefinition, returnType: TypeDefinition)

object Operators {
  val binaryOperators = List(
    BinaryOperationDefinition(Plus, PrimitiveIntType, PrimitiveIntType, PrimitiveIntType),
    BinaryOperationDefinition(Times, PrimitiveIntType, PrimitiveIntType, PrimitiveIntType),
    BinaryOperationDefinition(Minus, PrimitiveIntType, PrimitiveIntType, PrimitiveIntType),
    BinaryOperationDefinition(And, PrimitiveBooleanType, PrimitiveBooleanType, PrimitiveBooleanType),
    BinaryOperationDefinition(LessThan, PrimitiveIntType, PrimitiveIntType, PrimitiveBooleanType)
  )

  val unaryOperators = List(
    UnaryOperationDefinition(PrimitiveBooleanType, PrimitiveBooleanType) // Not
  )
}
