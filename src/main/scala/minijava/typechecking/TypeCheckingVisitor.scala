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

  override def visitStatementBlock(statementBlock: StatementBlock, a: TypeVisitorContext): TypeDefinition = {
    for (s <- statementBlock.statements) {
      visit(s, a)
    }

    PrimitiveVoidType
  }

  override def visitIfStatement(ifStatement: IfStatement, a: TypeVisitorContext): TypeDefinition = {
    val conditionType = visit(ifStatement.condition, a)

    val location = LineNumber(ifStatement.line)
    
    if (TypeDefinition.conformsTo(conditionType, PrimitiveBooleanType, a.typeTable).isEmpty) {
      failTypeCheck(conditionType, PrimitiveBooleanType, a.typeTable, location, "if statement condition")
    }

    visit(ifStatement.thenClause, a)
    visit(ifStatement.elseClause, a)

    PrimitiveVoidType
  }

  override def visitWhileStatement(whileStatement: WhileStatement, a: TypeVisitorContext): TypeDefinition = {
    val conditionType = visit(whileStatement.condition, a)

    val location = LineNumber(whileStatement.line)

    if (TypeDefinition.conformsTo(conditionType, PrimitiveBooleanType, a.typeTable).isEmpty) {
      failTypeCheck(conditionType, PrimitiveBooleanType, a.typeTable, location, "while statement condition")
    }

    visit(whileStatement.statement, a)

    PrimitiveVoidType
  }

  override def visitPrintStatement(printStatement: PrintStatement, a: TypeVisitorContext): TypeDefinition = {
    val expressionType = visit(printStatement.expression, a)

    val location = LineNumber(printStatement.line)

    TypeDefinition.conformsTo(expressionType, PrimitiveIntType, a.typeTable) match {
      case Some(t) =>
      case None => failTypeCheck(expressionType, PrimitiveIntType, a.typeTable, location, "print statement")
    }

    PrimitiveVoidType
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

  override def visitArrayAssignmentStatement(arrayAssignmentStatement: ArrayAssignmentStatement, a: TypeVisitorContext): TypeDefinition = {
    val indexType = visit(arrayAssignmentStatement.indexExpression, a)

    val location = LineNumber(arrayAssignmentStatement.line)

    TypeDefinition.conformsTo(indexType, PrimitiveIntType, a.typeTable) match {
      case Some(t) => t
      case None => failTypeCheck(indexType, PrimitiveIntType, a.typeTable, location, "array assignment index expression")
    }

    val valueType = visit(arrayAssignmentStatement.valueExpression, a)

    TypeDefinition.conformsTo(valueType, PrimitiveIntType, a.typeTable) match {
      case Some(t) => t
      case None => failTypeCheck(indexType, PrimitiveIntType, a.typeTable, location, "array assignment value expression")
    }

    val arrayType = getVarType(arrayAssignmentStatement.name.name, a)

    TypeDefinition.conformsTo(arrayType, PrimitiveIntArrayType, a.typeTable) match {
      case Some(t) => t
      case None => failTypeCheck(arrayType, PrimitiveIntArrayType, a.typeTable, location, "array assignment statement")
    }

    PrimitiveVoidType
  }

  override def visitBinaryOperationExpression(binaryOperationExpression: BinaryOperationExpression, a: TypeVisitorContext): TypeDefinition = {
    val firstType = visit(binaryOperationExpression.firstExpression, a)
    val secondType = visit(binaryOperationExpression.secondExpression, a)

    val operatorMatches = Operators.binaryOperators
      .filter(_.node == binaryOperationExpression.operator)

    val location = LineColumn(binaryOperationExpression.line, binaryOperationExpression.column)

    if (operatorMatches.nonEmpty) {
      val paramMatch = operatorMatches.filter(op =>
        TypeDefinition.conformsTo(firstType, op.firstParamType, a.typeTable).isDefined &&
        TypeDefinition.conformsTo(secondType, op.secondParamType, a.typeTable).isDefined
      )

      if (paramMatch.nonEmpty) {
        return paramMatch.head.returnType
      }

      val messageStart = "Found instance of %s with parameters of type \"%s\" and \"%s\". No version of the operation was found for this type combination.\n\nValid type combinations for this operator are:\n\n".format(
        binaryOperationExpression.operator.toSymbol(),
        firstType.getName(), secondType.getName()
      )

      val validTypes = for (op <- operatorMatches)
        yield "\"%s\" and \"%s\"".format(
          op.firstParamType.getName(), op.secondParamType.getName())

      val message = messageStart + validTypes.mkString("\n")

      val typeCheckError = CompilerMessage(CompilerError, TypeCheckingError, Some(location),
        message)

      typeCheckingErrors.append(typeCheckError)

      return FailType
    }

    ???
  }

  override def visitArrayAccessExpression(arrayAccessExpression: ArrayAccessExpression, a: TypeVisitorContext): TypeDefinition = {
    val indexType = visit(arrayAccessExpression.indexExpression, a)

    val location = LineColumn(arrayAccessExpression.line, arrayAccessExpression.column)

    val resultingIndexType = TypeDefinition.conformsTo(indexType, PrimitiveIntType, a.typeTable) match {
      case Some(t) => t
      case None => failTypeCheck(indexType, PrimitiveIntType, a.typeTable, location, "array access index expression")
    }

    val arrayType = visit(arrayAccessExpression.arrayExpression, a)

    val resultingArrayType = TypeDefinition.conformsTo(arrayType, PrimitiveIntArrayType, a.typeTable) match {
      case Some(t) => t
      case None => failTypeCheck(arrayType, PrimitiveIntArrayType, a.typeTable, location, "array access expression")
    }

    if (resultingIndexType == FailType || resultingArrayType == FailType) {
      FailType
    } else {
      PrimitiveIntType
    }
  }

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

  override def visitNewIntArrayExpression(intArrayExpression: NewIntArrayExpression, a: TypeVisitorContext): TypeDefinition = {
    val lengthType = visit(intArrayExpression.lengthExpression, a)

    val location = LineColumn(intArrayExpression.line, intArrayExpression.column)

    TypeDefinition.conformsTo(lengthType, PrimitiveIntType, a.typeTable) match {
      case Some(t) => PrimitiveIntArrayType
      case None => failTypeCheck(lengthType, PrimitiveIntType, a.typeTable, location, "new int array expression")
    }
  }

  override def visitNewObjectExpression(newObjectExpression: NewObjectExpression, a: TypeVisitorContext): TypeDefinition = {
    // TODO: What about constructor parameters?

    val className = newObjectExpression.className.name

    a.typeTable.get(className) match {
      case Some(t) => t
      case None => ???
    }
  }

  override def visitNegatedExpression(negatedExpression: NegatedExpression, a: TypeVisitorContext): TypeDefinition = {
    val expressionType = visit(negatedExpression.expression, a)

    val location = LineColumn(negatedExpression.line, negatedExpression.column)

    TypeDefinition.conformsTo(expressionType, PrimitiveBooleanType, a.typeTable) match {
      case Some(t) => t
      case None => failTypeCheck(expressionType, PrimitiveBooleanType, a.typeTable, location, "negated expression")
    }
  }

  override def visitParenedExpression(parenedExpression: ParenedExpression, a: TypeVisitorContext): TypeDefinition = {
    visit(parenedExpression.expression, a)
  }

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

    getVarTypeClass(name, context.curClass, context.typeTable)
  }

  private def getVarTypeClass(name: String, classLikeType: ClassLikeType, typeTable: TypeTable): TypeDefinition = {
    val classNameMatches = classLikeType.getVariables()
      .filter(cv => cv.name == name)

    classNameMatches.headOption match {
      case Some(t) => return typeTable.get(t.typeName).get
      case None =>
    }

    classLikeType.getParentClass() match {
      case Some(p) =>
        typeTable.get(p).get match {
          case parent: ClassLikeType => getVarTypeClass(name, parent, typeTable)
          case _ => ???
        }
      case None => ???
    }
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
