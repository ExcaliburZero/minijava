package minijava.grammar

sealed trait ASTNode

case class Goal(mainClass: MainClass, classDeclarations: List[ClassDeclaration]) extends ASTNode

case class MainClass(name: Identifier, isIO: Boolean, parameter: Identifier, statement: Statement) extends ASTNode

case class ClassDeclaration(name: Identifier, parentClass: Option[Identifier], variableDeclarations: List[VariableDeclaration], methodDeclarations: List[MethodDeclaration]) extends ASTNode

case class VariableDeclaration(varType: Type, name: Identifier) extends ASTNode

case class MethodDeclaration(isIO: Boolean, varType: Type, name: Identifier, parameters: List[(Type, Identifier)], variableDeclarations: List[VariableDeclaration], statements: List[Statement], returnExpression: Expression) extends ASTNode

sealed trait Type extends ASTNode
case object IntArrayType extends Type
case object BooleanType extends Type
case object IntType extends Type

sealed trait Statement extends ASTNode
case class StatementBlock(statements: List[Statement]) extends Statement
case class IfStatement(condition: Expression, thenClause: Statement, elseClause: Statement) extends Statement
case class WhileStatement(condition: Expression, statements: Statement) extends Statement
case class PrintStatement(expression: Expression) extends Statement
case class AssignmentStatement(name: Identifier, expression: Expression) extends Statement
case class ArrayAssignmentStatement(name: Identifier, indexExpression: Expression, valueExpression: Expression) extends Statement

sealed trait Expression extends ASTNode
case class BinaryOperationExpression(firstExpression: Expression, operator: BinaryOperator, secondExpression: Expression) extends Expression
case class ArrayAccessExpression(arrayExpression: Expression, indexExpression: Expression) extends Expression
case class ArrayLengthExpression(arrayExpression: Expression) extends Expression
case class MethodCallExpression(objectExpression: Expression, methodName: Identifier, parameters: List[Expression]) extends Expression
case class IntegerLiteral(value: Int) extends Expression
case object TrueLiteral extends Expression
case object FalseLiteral extends Expression
case class IdentifierExpression(name: Identifier) extends Expression
case object ThisLiteral extends Expression
case class NewIntArrayExpression(lengthExpression: Expression) extends Expression
case class NewObjectExpression(className: Identifier) extends Expression
// TODO: more

sealed trait BinaryOperator
case object And extends BinaryOperator
case object LessThan extends BinaryOperator
case object Plus extends BinaryOperator
case object Minus extends BinaryOperator
case object Times extends BinaryOperator
case object Not extends BinaryOperator

case class Identifier(name: String) extends Type

object AST {
  private val INDENT = "    "

  def prettyPrint(node: ASTNode): String = {
    val sb = new StringBuilder()

    prettyPrint(node, sb, 0)

    sb.toString()
  }

  def prettyPrint(node: ASTNode, sb: StringBuilder, indentLevel: Int): Unit = {
    val indent = Array.fill(indentLevel)("    ").mkString("")
    node match {
      case Goal(mainClass, classDeclarations) => {
        prettyPrint(mainClass, sb, indentLevel)
        sb.append("\n")

        classDeclarations.foreach(cd => {
          prettyPrint(cd, sb, indentLevel)
          sb.append("\n")
        })
      }
      case MainClass(name, isIO, parameter, statement) => {
        sb.append("%sclass %s {\n".format(indent, name.name))

        val ioKeyword = if (isIO) "io " else ""

        sb.append("%spublic static %svoid main(String[] %s) {\n".format(indent + INDENT, ioKeyword, parameter.name))

        prettyPrint(statement, sb, indentLevel + 2)

        sb.append("%s}\n".format(indent + INDENT))

        sb.append("%s}\n".format(indent))
      }
      case ClassDeclaration(name, parentClass, variableDeclarations, methodDeclarations) => {
        sb.append("class %s ".format(name.name))

        parentClass.foreach("extends %s".format(_))

        sb.append("{\n")

        variableDeclarations.foreach(prettyPrint(_, sb, indentLevel + 1))
        methodDeclarations.foreach(prettyPrint(_, sb, indentLevel + 1))

        sb.append("}\n")
      }
      case MethodDeclaration(isIO, varType, name, parameters, variableDeclarations, statements, returnExpression) => {
        sb.append("%spublic ".format(indent))

        if (isIO) sb.append("io ")

        prettyPrint(varType, sb, indentLevel)

        sb.append(" %s(".format(name.name))

        parameters.headOption.foreach(v => {
          prettyPrint(v._1, sb, indentLevel)

          sb.append(" %s".format(v._2.name))
        })
        parameters.tail.foreach(v => {
          sb.append(", ")

          prettyPrint(v._1, sb, indentLevel)

          sb.append(" %s".format(v._2.name))
        })

        sb.append(") {\n")

        sb.append("%s}\n".format(indent))
      }
      case IntType => sb.append("int")
      case PrintStatement(expression) => {
        sb.append("%sSystem.out.println(".format(indent))

        prettyPrint(expression, sb, indentLevel)

        sb.append(");\n")
      }
      case MethodCallExpression(objectExpression, methodName, parameters) => {
        prettyPrint(objectExpression, sb, indentLevel)

        sb.append(".%s(".format(methodName.name))

        parameters.headOption.foreach(prettyPrint(_, sb, indentLevel))
        parameters.tail.foreach(p => {
          sb.append(", ")

          prettyPrint(p, sb, indentLevel)
        })

        sb.append(")")
      }
      case IntegerLiteral(value) => sb.append(value)
      case NewObjectExpression(className) => {
        sb.append("new %s()".format(className.name))
      }
      case _ => sb.append("TODO")
    }
  }
}
