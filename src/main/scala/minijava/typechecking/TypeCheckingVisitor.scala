package minijava.typechecking

import minijava.grammar._
import minijava.messages.{CompilerMessage, LineNumber, Location}

import scala.collection.mutable.ArrayBuffer

case class TypeVisitorContext(typeTable: TypeTable, curClass: ClassType, curMethod: Method)

class TypeCheckingVisitor extends ASTVisitor[TypeVisitorContext, TypeDefinition] {
  private val typeCheckingErrors = new ArrayBuffer[CompilerMessage]()

  def getTypeCheckingErrors(): List[CompilerMessage] = {
    typeCheckingErrors.toList
  }

  override def visitStatementBlock(statementBlock: StatementBlock, a: TypeVisitorContext): TypeDefinition = super.visitStatementBlock(statementBlock, a)

  override def visitIfStatement(ifStatement: IfStatement, a: TypeVisitorContext): TypeDefinition = super.visitIfStatement(ifStatement, a)

  override def visitWhileStatement(whileStatement: WhileStatement, a: TypeVisitorContext): TypeDefinition = super.visitWhileStatement(whileStatement, a)

  override def visitPrintStatement(printStatement: PrintStatement, a: TypeVisitorContext): TypeDefinition = super.visitPrintStatement(printStatement, a)

  override def visitAssignmentStatement(assignmentStatement: AssignmentStatement, a: TypeVisitorContext): TypeDefinition = {
    val expressionType = visit(assignmentStatement.expression, a)
    val variableType = getVarType(assignmentStatement.name.name, a)

    val location = LineNumber(assignmentStatement.line)

    TypeDefinition.conformsTo(expressionType, variableType, a.typeTable) match {
      case Some(t) => t
      case None => failTypeCheck(expressionType, variableType, a.typeTable, location)
    }
  }

  override def visitArrayAssignmentStatement(arrayAssignmentStatement: ArrayAssignmentStatement, a: TypeVisitorContext): TypeDefinition = super.visitArrayAssignmentStatement(arrayAssignmentStatement, a)

  override def visitBinaryOperationExpression(binaryOperationExpression: BinaryOperationExpression, a: TypeVisitorContext): TypeDefinition = super.visitBinaryOperationExpression(binaryOperationExpression, a)

  override def visitArrayAccessExpression(arrayAccessExpression: ArrayAccessExpression, a: TypeVisitorContext): TypeDefinition = super.visitArrayAccessExpression(arrayAccessExpression, a)

  override def visitArrayLengthExpression(arrayLengthExpression: ArrayLengthExpression, a: TypeVisitorContext): TypeDefinition = super.visitArrayLengthExpression(arrayLengthExpression, a)

  override def visitMethodCallExpression(methodCallExpression: MethodCallExpression, a: TypeVisitorContext): TypeDefinition = {
    val objectType = visit(methodCallExpression.objectExpression, a)

    val classType = objectType match {
      case _: ClassType =>
      case FailType => return FailType
      case _ => return failMethodCallOnNonObject(objectType)
    }

    val parameterTypes = for (p <- methodCallExpression.parameters)
      yield visit(p, a)

    val methodName = methodCallExpression.methodName.name

    getMatchingMethod(objectType.asInstanceOf[ClassType], methodName, parameterTypes, a) match {
      case None => ???
      case Some(m) => a.typeTable.get(m.returnType).get
    }
  }

  override def visitIntegerLiteral(integerLiteral: IntegerLiteral, a: TypeVisitorContext): TypeDefinition = super.visitIntegerLiteral(integerLiteral, a)

  override def visitTrueLiteral(a: TypeVisitorContext): TypeDefinition = super.visitTrueLiteral(a)

  override def visitFalseLiteral(a: TypeVisitorContext): TypeDefinition = super.visitFalseLiteral(a)

  override def visitIdentifierExpression(identifierExpression: IdentifierExpression, a: TypeVisitorContext): TypeDefinition = {
    getVarType(identifierExpression.name.name, a)
  }

  override def visitThisLiteral(a: TypeVisitorContext): TypeDefinition = {
    a.curClass
  }

  override def visitNewIntArrayExpression(intArrayExpression: NewIntArrayExpression, a: TypeVisitorContext): TypeDefinition = super.visitNewIntArrayExpression(intArrayExpression, a)

  override def visitNewObjectExpression(newObjectExpression: NewObjectExpression, a: TypeVisitorContext): TypeDefinition = super.visitNewObjectExpression(newObjectExpression, a)

  override def visitNegatedExpression(negatedExpression: NegatedExpression, a: TypeVisitorContext): TypeDefinition = super.visitNegatedExpression(negatedExpression, a)

  override def visitParenedExpression(parenedExpression: ParenedExpression, a: TypeVisitorContext): TypeDefinition = super.visitParenedExpression(parenedExpression, a)

  def getVarType(name: String, context: TypeVisitorContext): TypeDefinition = {
    // TODO: somewhere check for shadowed names
    //context.curMethod.parameters ++ context.curMethod.localVariables

    val localNameMatches = context.curMethod.localVariables
      .filter(lv => lv.name == name)

    localNameMatches.headOption match {
      case Some(t) => return context.typeTable.get(t.typeName).get
      case None =>
    }

    val paramNameMatches = context.curMethod.parameters
      .filter(lv => lv.name == name)

    paramNameMatches.headOption match {
      case Some(t) => return context.typeTable.get(t.typeName).get
      case None =>
    }

    ???
  }

  def failTypeCheck(expressionType: TypeDefinition, variableType: TypeDefinition, typeTable: TypeTable, location: Location): TypeDefinition = {
    val message = "Incompatible types.\nExpected: %s\nFound:%s".format(variableType.getName(), expressionType.getName())

    val typeCheckError = ???

    typeCheckingErrors.append(typeCheckError)

    FailType
  }

  def failMethodCallOnNonObject(objectType: TypeDefinition): TypeDefinition = {
    val message = ???

    ???
  }

  def getMatchingMethod(objectType: ClassType, methodName: String, parameterTypes: List[TypeDefinition], a: TypeVisitorContext): Option[Method] = {
    val classNameMatches = objectType.methods
        .filter(m => m.name == methodName)

    val classParamMatches = classNameMatches.filter(m => {
      val mParamTypes = m.parameters.map(p => a.typeTable.get(p.typeName).get)
      val paramMatches = for ((mt, pt) <- mParamTypes.zip(parameterTypes))
        yield TypeDefinition.conformsTo(pt, mt, a.typeTable).isEmpty

      paramMatches.isEmpty || paramMatches.contains(false)
    })

    classParamMatches.headOption match {
      case Some(m) => return Some(m)
      case None =>
    }

    ???
  }
}
