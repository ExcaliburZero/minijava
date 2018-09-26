case class Goal(mainClass: MainClass, classDeclarations: List[ClassDeclaration])

case class MainClass(name: Identifier, parentClass: Option[Identifier], isIO: Boolean, parameter: Identifier, statements: List[Statement])

case class ClassDeclaration(name: Identifier, parentClass: Option[Identifier], variableDeclarations: List[VariableDeclaration], methodDeclarations: List[MethodDeclaration])

case class VariableDeclaration(varType: Type, name: Identifier)

case class MethodDeclaration(isIO: Boolean, varType: Type, name: Identifier, parameters: List[(Type, Identifier)], variableDeclarations: List[VariableDeclaration], statements: List[Statement], returnExpression: Expression)

sealed trait Type
case object IntArray extends Type
case object BooleanType extends Type
case object IntType extends Type

sealed trait Statement
case class StatementBlock(statements: List[Statement]) extends Statement
// TODO: more

sealed trait Expression
// TODO: more

case class Identifier(name: String) extends Type
