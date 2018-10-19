package minijava.grammar

sealed trait ASTNode

case class Goal(mainClass: MainClass, regularClasses: List[RegularClass]) extends ASTNode

sealed trait ClassDeclaration extends ASTNode
case class MainClass(name: Identifier, isIO: Boolean, parameter: Identifier, statement: Statement, line: Int) extends ClassDeclaration
case class RegularClass(name: Identifier, parentClass: Option[Identifier], variableDeclarations: List[VariableDeclaration], methodDeclarations: List[MethodDeclaration], line: Int) extends ClassDeclaration

//case class MainClass(name: Identifier, isIO: Boolean, parameter: Identifier, statement: Statement, line: Int) extends ASTNode

//case class ClassDeclaration(name: Identifier, parentClass: Option[Identifier], variableDeclarations: List[VariableDeclaration], methodDeclarations: List[MethodDeclaration], line: Int) extends ASTNode

case class VariableDeclaration(varType: Type, name: Identifier, line: Int) extends ASTNode

case class MethodDeclaration(isIO: Boolean, varType: Type, name: Identifier, parameters: List[(Type, Identifier)], variableDeclarations: List[VariableDeclaration], statements: List[Statement], returnExpression: Expression, line: Int) extends ASTNode

sealed trait Type extends ASTNode
case object IntArrayType extends Type
case object BooleanType extends Type
case object IntType extends Type
case class IdentifierType(name: Identifier) extends Type

sealed trait Statement extends ASTNode
case class StatementBlock(statements: List[Statement]) extends Statement
case class IfStatement(condition: Expression, thenClause: Statement, elseClause: Statement, line: Int) extends Statement
case class WhileStatement(condition: Expression, statement: Statement, line: Int) extends Statement
case class PrintStatement(expression: Expression, line: Int) extends Statement
case class AssignmentStatement(name: Identifier, expression: Expression, line: Int) extends Statement
case class ArrayAssignmentStatement(name: Identifier, indexExpression: Expression, valueExpression: Expression, line: Int) extends Statement

sealed trait Expression extends ASTNode
case class BinaryOperationExpression(firstExpression: Expression, operator: BinaryOperator, secondExpression: Expression, line: Int, column: Int) extends Expression
case class ArrayAccessExpression(arrayExpression: Expression, indexExpression: Expression, line: Int, column: Int) extends Expression
case class ArrayLengthExpression(arrayExpression: Expression) extends Expression
case class MethodCallExpression(objectExpression: Expression, methodName: Identifier, parameters: List[Expression]) extends Expression
case class IntegerLiteral(value: Int) extends Expression
case object TrueLiteral extends Expression
case object FalseLiteral extends Expression
case class IdentifierExpression(name: Identifier, line: Int, column: Int) extends Expression
case object ThisLiteral extends Expression
case class NewIntArrayExpression(lengthExpression: Expression, line: Int, column: Int) extends Expression
case class NewObjectExpression(className: Identifier) extends Expression
case class NegatedExpression(expression: Expression, line: Int, column: Int) extends Expression
case class ParenedExpression(expression: Expression) extends Expression

sealed trait BinaryOperator {
  def toSymbol(): String = {
    this match {
      case And => "&&"
      case LessThan => "<"
      case Plus => "+"
      case Minus => "-"
      case Times => "*"
    }
  }
}
case object And extends BinaryOperator
case object LessThan extends BinaryOperator
case object Plus extends BinaryOperator
case object Minus extends BinaryOperator
case object Times extends BinaryOperator

case class Identifier(name: String)

object AST {
  /*private val INDENT = "    "

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

        classDeclarations.foreach(cd => {
          sb.append("\n")
          prettyPrint(cd, sb, indentLevel)
        })
      }
      case MainClass(name, isIO, parameter, statement, _) => {
        sb.append("%sclass %s {\n".format(indent, name.name))

        val ioKeyword = if (isIO) "io " else ""

        sb.append("%spublic static %svoid main(String[] %s) {\n".format(indent + INDENT, ioKeyword, parameter.name))

        prettyPrint(statement, sb, indentLevel + 2)

        sb.append("%s}\n".format(indent + INDENT))

        sb.append("%s}\n".format(indent))
      }
      case ClassDeclaration(name, parentClass, variableDeclarations, methodDeclarations, _) => {
        sb.append("class %s ".format(name.name))

        parentClass.foreach(p => sb.append("extends %s".format(p.name)))

        sb.append("{\n")

        variableDeclarations.foreach(prettyPrint(_, sb, indentLevel + 1))
        methodDeclarations.foreach(prettyPrint(_, sb, indentLevel + 1))

        sb.append("}\n")
      }
      case VariableDeclaration(varType, name) => {
        sb.append(indent)

        prettyPrint(varType, sb, indentLevel)

        sb.append(" %s;\n".format(name.name))
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
        parameters.drop(1).foreach(v => {
          sb.append(", ")

          prettyPrint(v._1, sb, indentLevel)

          sb.append(" %s".format(v._2.name))
        })

        sb.append(") {\n")

        variableDeclarations.foreach(prettyPrint(_, sb, indentLevel + 1))
        statements.foreach(prettyPrint(_, sb, indentLevel + 1))

        sb.append("%sreturn ".format(indent + INDENT))
        prettyPrint(returnExpression, sb, indentLevel)
        sb.append(";\n")

        sb.append("%s}\n".format(indent))
      }
      case IntArrayType => sb.append("int[]")
      case BooleanType => sb.append("boolean")
      case IntType => sb.append("int")
      case IdentifierType(name) => sb.append(name.name)
      case StatementBlock(statements) => {
        sb.append("%s{\n".format(indent))

        statements.foreach(s => {
          prettyPrint(s, sb, indentLevel + 1)
        })

        sb.append("%s}\n".format(indent))
      }
      case IfStatement(condition, thenClause, elseClause) => {
        sb.append("%sif (".format(indent))

        prettyPrint(condition, sb, indentLevel)

        sb.append(")\n")

        prettyPrint(thenClause, sb, indentLevel + 1)

        sb.append("%selse\n".format(indent))

        prettyPrint(elseClause, sb, indentLevel + 1)

        sb.append("\n")
      }
      case WhileStatement(condition, statement) => {
        sb.append("%swhile(".format(indent))

        prettyPrint(condition, sb, indentLevel)

        sb.append(")\n")

        prettyPrint(statement, sb, indentLevel + 1)
      }
      case PrintStatement(expression) => {
        sb.append("%sSystem.out.println(".format(indent))

        prettyPrint(expression, sb, indentLevel)

        sb.append(");\n")
      }
      case AssignmentStatement(name, expression) => {
        sb.append("%s%s = ".format(indent, name.name))

        prettyPrint(expression, sb, indentLevel)

        sb.append(";\n")
      }
      case ArrayAssignmentStatement(name, indexExpression, valueExpression) => {
        sb.append("%s%s[".format(indent, name.name))

        prettyPrint(indexExpression, sb, indentLevel)

        sb.append("] = ")

        prettyPrint(valueExpression, sb, indentLevel)

        sb.append(";\n")
      }
      case BinaryOperationExpression(firstExpression, operator, secondExpression) => {
        val operatorString = operator match {
          case And => "&&"
          case LessThan => "<"
          case Plus => "+"
          case Minus => "-"
          case Times => "*"
        }

        prettyPrint(firstExpression, sb, indentLevel)

        sb.append(" %s ".format(operatorString))

        prettyPrint(secondExpression, sb, indentLevel)
      }
      case ArrayAccessExpression(arrayExpression, indexExpression) => {
        prettyPrint(arrayExpression, sb, indentLevel)

        sb.append("[")

        prettyPrint(indexExpression, sb, indentLevel)

        sb.append("]")
      }
      case ArrayLengthExpression(arrayExpression) => {
        prettyPrint(arrayExpression, sb, indentLevel)

        sb.append(".length")
      }
      case MethodCallExpression(objectExpression, methodName, parameters) => {
        prettyPrint(objectExpression, sb, indentLevel)

        sb.append(".%s(".format(methodName.name))

        parameters.headOption.foreach(prettyPrint(_, sb, indentLevel))
        parameters.drop(1).foreach(p => {
          sb.append(", ")

          prettyPrint(p, sb, indentLevel)
        })

        sb.append(")")
      }
      case IntegerLiteral(value) => sb.append(value)
      case TrueLiteral => sb.append("true")
      case FalseLiteral => sb.append("false")
      case IdentifierExpression(identifier) => sb.append(identifier.name)
      case ThisLiteral => sb.append("this")
      case NewIntArrayExpression(lengthExpression) => {
        sb.append("new int[")

        prettyPrint(lengthExpression, sb, indentLevel)

        sb.append("]")
      }
      case NewObjectExpression(className) => {
        sb.append("new %s()".format(className.name))
      }
      case NegatedExpression(expression) => {
        sb.append("!")

        prettyPrint(expression, sb, indentLevel)
      }
      case ParenedExpression(expression) => {
        sb.append("(")

        prettyPrint(expression, sb, indentLevel)

        sb.append(")")
      }
      case _ => sb.append("TODO")
    }
  }*/
}
