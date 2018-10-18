package minijava.typechecking

import minijava.grammar._
import minijava.messages._

import scala.collection.mutable.ArrayBuffer

case class TypeVisitorContext(typeTable: TypeTable, curClass: ClassLikeType, curMethod: Method)

class TypeCheckingVisitor extends ASTVisitor[TypeVisitorContext, TypeDefinition] {
  private val typeCheckingErrors = new ArrayBuffer[CompilerMessage]()

  def getTypeCheckingErrors(): List[CompilerMessage] = {
    typeCheckingErrors.toList
  }

  override def visitStatementBlock(statementBlock: StatementBlock, a: TypeVisitorContext): TypeDefinition = super.visitStatementBlock(statementBlock, a)

  override def visitIfStatement(ifStatement: IfStatement, a: TypeVisitorContext): TypeDefinition = {
    val conditionType = visit(ifStatement.condition, a)

    val location = LineNumber(ifStatement.line)
    
    if (conditionType != PrimitiveBooleanType) {
      failTypeCheck(conditionType, PrimitiveBooleanType, a.typeTable, location, "if statement")
    }

    visit(ifStatement.thenClause, a)
    visit(ifStatement.elseClause, a)

    PrimitiveVoidType
  }

  override def visitWhileStatement(whileStatement: WhileStatement, a: TypeVisitorContext): TypeDefinition = super.visitWhileStatement(whileStatement, a)

  override def visitPrintStatement(printStatement: PrintStatement, a: TypeVisitorContext): TypeDefinition = {
    val expressionType = visit(printStatement.expression, a)

    val location = LineNumber(printStatement.line)

    TypeDefinition.conformsTo(expressionType, PrimitiveIntType, a.typeTable) match {
      case Some(t) => t
      case None => failTypeCheck(expressionType, PrimitiveIntType, a.typeTable, location, "print statement")
    }
  }

  override def visitAssignmentStatement(assignmentStatement: AssignmentStatement, a: TypeVisitorContext): TypeDefinition = {
    val expressionType = visit(assignmentStatement.expression, a)
    val variableType = getVarType(assignmentStatement.name.name, a)

    val location = LineNumber(assignmentStatement.line)

    TypeDefinition.conformsTo(expressionType, variableType, a.typeTable) match {
      case Some(t) => t
      case None => failTypeCheck(expressionType, variableType, a.typeTable, location, "assignment statement")
    }
  }

  override def visitArrayAssignmentStatement(arrayAssignmentStatement: ArrayAssignmentStatement, a: TypeVisitorContext): TypeDefinition = super.visitArrayAssignmentStatement(arrayAssignmentStatement, a)

  override def visitBinaryOperationExpression(binaryOperationExpression: BinaryOperationExpression, a: TypeVisitorContext): TypeDefinition = {
    val firstType = visit(binaryOperationExpression.firstExpression, a)
    val secondType = visit(binaryOperationExpression.secondExpression, a)

    ???
  }

  override def visitArrayAccessExpression(arrayAccessExpression: ArrayAccessExpression, a: TypeVisitorContext): TypeDefinition = super.visitArrayAccessExpression(arrayAccessExpression, a)

  override def visitArrayLengthExpression(arrayLengthExpression: ArrayLengthExpression, a: TypeVisitorContext): TypeDefinition = {
    PrimitiveIntType
  }

  override def visitMethodCallExpression(methodCallExpression: MethodCallExpression, a: TypeVisitorContext): TypeDefinition = {
    val objectType = visit(methodCallExpression.objectExpression, a)

    objectType match {
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

  override def visitIntegerLiteral(integerLiteral: IntegerLiteral, a: TypeVisitorContext): TypeDefinition = {
    PrimitiveIntType
  }

  override def visitTrueLiteral(a: TypeVisitorContext): TypeDefinition = {
    PrimitiveBooleanType
  }

  override def visitFalseLiteral(a: TypeVisitorContext): TypeDefinition = {
    PrimitiveBooleanType
  }

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

  def failTypeCheck(actualType: TypeDefinition, expectedType: TypeDefinition, typeTable: TypeTable, location: Location, context: String): TypeDefinition = {
    val message = "Incompatible types in %s.\nExpected:  %s\nFound:     %s".format(context, expectedType.getName(), actualType.getName())

    val typeCheckError = CompilerMessage(CompilerError, TypeCheckingError, Some(location),
      message)

    typeCheckingErrors.append(typeCheckError)

    FailType
  }

  def failMethodCallOnNonObject(objectType: TypeDefinition): TypeDefinition = {
    val message = ???

    ???
  }

  def getMatchingMethod(objectType: ClassLikeType, methodName: String, parameterTypes: List[TypeDefinition], a: TypeVisitorContext): Option[Method] = {
    val classNameMatches = objectType.getMethods()
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
