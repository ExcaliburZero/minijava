package minijava.typechecking

import minijava.grammar._
import minijava.messages._

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

case class TypeVisitorContext(typeTable: TypeTable, curClass: ClassLikeType, curMethod: Method)

/**
  * A TypeCheckingVisitor is used to type check the AST of a program, given information about the known types of the
  * program. It assumes that the types of the program have already been extracted and that class inheritance and
  * overloading issues have already been handled.
  *
  * It should only be used to visit statements and smaller portions of the AST. Using it to visit something higher, like
  * a MainClass, will result in a NotImplementedError being thrown.
  */
class TypeCheckingVisitor extends ASTVisitor[TypeVisitorContext, TypeDefinition] {
  private val typeCheckingErrors = new ArrayBuffer[CompilerMessage]()

  def getTypeCheckingErrors(): List[CompilerMessage] = {
    typeCheckingErrors.toList
  }

  def checkReturnType(expectedType: TypeDefinition, returnExpression: Expression, a: TypeVisitorContext): Unit = {
    val actualType = visit(returnExpression, a)

    if (TypeDefinition.conformsTo(actualType, expectedType, a.typeTable).isEmpty) {
      failReturnType(actualType, expectedType, a)
    }
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
    val location = LineNumber(assignmentStatement.line)

    val expressionType = visit(assignmentStatement.expression, a)
    val (variableType, variableContext) = getVarType(assignmentStatement.name.name, a.curMethod, a.curClass, a.typeTable, location)

    // TODO: store variable context

    TypeDefinition.conformsTo(expressionType, variableType, a.typeTable) match {
      case Some(t) => t
      case None => failTypeCheck(expressionType, variableType, a.typeTable, location,
        "assignment statement for variable \"%s\"".format(assignmentStatement.name.name))
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
      case None => failTypeCheck(valueType, PrimitiveIntType, a.typeTable, location, "array assignment value expression")
    }

    val (arrayType, arrayContext) = getVarType(arrayAssignmentStatement.name.name, a.curMethod, a.curClass, a.typeTable, location)

    // TODO: store array context

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
        paramMatch.head.returnType
      } else {
        val operatorSymbol = binaryOperationExpression.operator.toSymbol()

        failUnknownOperatorTypes(operatorSymbol, firstType, secondType, operatorMatches, location)
      }
    } else {
      // An unknown operator was given. This should not be possible since the grammar only allows the use of known
      // operators.
      ???
    }
  }

  override def visitArrayAccessExpression(arrayAccessExpression: ArrayAccessExpression, a: TypeVisitorContext): TypeDefinition = {
    val location = LineColumn(arrayAccessExpression.line, arrayAccessExpression.column)

    val indexType = visit(arrayAccessExpression.indexExpression, a)
    val indexExpected = PrimitiveIntType
    val resultingIndexType = TypeDefinition.conformsTo(indexType, indexExpected, a.typeTable) match {
      case Some(t) => t
      case None => failTypeCheck(indexType, indexExpected, a.typeTable, location, "array access index expression")
    }

    val arrayType = visit(arrayAccessExpression.arrayExpression, a)
    val arrayExpected = PrimitiveIntArrayType
    val resultingArrayType = TypeDefinition.conformsTo(arrayType, arrayExpected, a.typeTable) match {
      case Some(t) => t
      case None => failTypeCheck(arrayType, arrayExpected, a.typeTable, location, "array access expression")
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

    methodCallExpression.classType = Some(objectType)

    val methodName = methodCallExpression.methodName.name

    val location = LineColumn(methodCallExpression.line, methodCallExpression.column)

    objectType match {
      case _: ClassType =>
        val parameterTypes = for (p <- methodCallExpression.parameters)
          yield visit(p, a)

        getMatchingMethod(objectType.asInstanceOf[ClassType], methodName, parameterTypes, a) match {
          case Some(m) =>
            methodCallExpression.method = Some(m)
            a.typeTable.get(m.returnType).get
          case None => failUnknownMethod(objectType.getName(), methodName, parameterTypes, location)
        }
      case FailType => FailType
      case _ => failMethodCallOnNonObject(objectType, methodName, location)
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
    val varName = identifierExpression.name.name
    val location = LineColumn(identifierExpression.line, identifierExpression.column)

    val (varType, varContext) = getVarType(varName, a.curMethod, a.curClass, a.typeTable, location)

    identifierExpression.context = varContext

    varType
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
    val className = newObjectExpression.className.name

    val location = LineColumn(newObjectExpression.line, newObjectExpression.column)

    a.typeTable.get(className) match {
      case Some(t) => t
      case None => failInstantiateUnknownClass(className, location)
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

  /**
    * Returns the declared type of the specified variable by looking at the method and class context where it is used.
    *
    * @param name The name of the variable.
    * @param curMethod The method the variable is referenced in.
    * @param curClass The class the variable is referenced in.
    * @param typeTable The type table.
    * @param location The location in the code that this check is for. (used for error messages)
    * @return Some variable type if one is found, else None
    */
  private def getVarType(name: String, curMethod: Method, curClass: ClassLikeType, typeTable: TypeTable, location: Location): (TypeDefinition, Option[VariableContext]) = {
    // Look at the local variables
    val localNameMatches = curMethod.localVariables
      .filter(lv => lv.name == name)

    localNameMatches.headOption match {
      case Some(t) => return (typeTable.get(t.typeName).get, Some(MethodVariable(curMethod, LocalVariable)))
      case None =>
    }

    // If it is not a local variable, then check if it is a parameter
    val paramNameMatches = curMethod.parameters
      .filter(lv => lv.name == name)

    paramNameMatches.headOption match {
      case Some(t) => (typeTable.get(t.typeName).get, Some(MethodVariable(curMethod, Parameter)))
      case None =>
        // If it is not a parameter, then check if it is a class variable
        getVarTypeClass(name, curClass, typeTable, location)
    }
  }

  /**
    * Returns the declared type of the specified class variable by looking at the class' known variables as well as its
    * ancestors'.
    *
    * @param name The name of the variable.
    * @param classLikeType The class type to look for the variable in.
    * @param typeTable The type table.
    * @param location The location in the code that this check is for. (used for error messages)
    * @return Some variable type if one is found, else None
    */
  @tailrec
  private def getVarTypeClass(name: String, classLikeType: ClassLikeType, typeTable: TypeTable, location: Location): (TypeDefinition, Option[VariableContext]) = {
    // Look at the variables for this class
    val classNameMatches = classLikeType.getVariables()
      .filter(cv => cv.name == name)

    classNameMatches.headOption match {
      case Some(t) => return (typeTable.get(t.typeName).get, Some(classLikeType))
      case None =>
    }

    // If the variable is not present in the class, then look at its parent
    classLikeType.getParentClass() match {
      case Some(p) =>
        typeTable.get(p).get match {
          case parent: ClassLikeType => getVarTypeClass(name, parent, typeTable, location)
          case _ => ??? // The parent is not a class, this is handled in the (earlier) type extraction phase
        }
      case None =>
        // If there is no parent class to check, then it must be an unknown variable
        (failVariableNotFound(name, location), None)
    }
  }

  /**
    * Returns the method for the given class with the given name and parameter types. In the case that no such method is
    * found, then a None is returned.
    *
    * Looks first at the given class type, and then looks at all parent classes as well. Allows methods with parameter
    * types that conform to the specified parameter types.
    *
    * @param objectType The class type of the object that the method is being called on.
    * @param methodName The name of the method to get.
    * @param parameterTypes The parameter types that the method needs to conform to.
    * @param a The type checking context.
    * @return Some matching method if one is found, else None
    */
  @tailrec
  private def getMatchingMethod(objectType: ClassLikeType, methodName: String, parameterTypes: List[TypeDefinition], a: TypeVisitorContext): Option[Method] = {
    val classNameMatches = objectType.getMethods()
        .filter(m => m.name == methodName)

    val classParamMatches = classNameMatches.filter(m => {
      val mParamTypes = m.parameters.map(p => a.typeTable.get(p.typeName).get)

      // The methods have to have the same parameter arity
      if (mParamTypes.length == parameterTypes.length) {
        val pairs = mParamTypes.zip(parameterTypes)
        val paramMatches = for ((mt, pt) <- pairs)
          yield TypeDefinition.conformsTo(pt, mt, a.typeTable).isDefined

        // The methods match if they either have no parameters, or all of their parameters conform
        paramMatches.isEmpty || !paramMatches.contains(false)
      } else {
        // If the method does not have the same arity, then it cannot be the correct one
        false
      }
    })

    // If a match was found, then it should be returned
    classParamMatches.headOption match {
      case Some(m) => Some(m)
      case None =>
        // If no matches were found in this class, then check its parent
        objectType.getParentClass() match {
          case Some(p) =>
            getMatchingMethod(a.typeTable.get(p).get.asInstanceOf[ClassLikeType],
              methodName, parameterTypes, a)
          case None =>
            // If there is no parent class to check, then the method must be unknown
            None
        }
    }
  }

  private def failTypeCheck(actualType: TypeDefinition, expectedType: TypeDefinition, typeTable: TypeTable, location: Location, context: String): TypeDefinition = {
    val message = "Incompatible types in %s.\nExpected:  %s\nFound:     %s".format(context, expectedType.getName(), actualType.getName())

    val typeCheckError = CompilerMessage(CompilerError, TypeCheckingError, Some(location),
      message)

    typeCheckingErrors.append(typeCheckError)

    FailType
  }

  private def failMethodCallOnNonObject(objectType: TypeDefinition, methodName: String, location: Location): TypeDefinition = {
    val message = "Method \"%s\" called on non-object value of type \"%s\"."
      .format(methodName, objectType.getName())

    val typeCheckError = CompilerMessage(CompilerError, TypeCheckingError, Some(location),
      message)

    typeCheckingErrors.append(typeCheckError)

    FailType
  }

  private def failVariableNotFound(name: String, location: Location): TypeDefinition = {
    val message = "Unknown variable \"%s\", perhaps it has not been declared.".format(name)

    val typeCheckError = CompilerMessage(CompilerError, TypeCheckingError, Some(location),
      message)

    typeCheckingErrors.append(typeCheckError)

    FailType
  }

  private def failInstantiateUnknownClass(className: String, location: Location): TypeDefinition = {
    val message = "Instantiation of unknown class \"%s\"."
      .format(className)

    val typeCheckError = CompilerMessage(CompilerError, TypeCheckingError, Some(location),
      message)

    typeCheckingErrors.append(typeCheckError)

    FailType
  }

  private def failUnknownMethod(className: String, methodName: String, parameterTypes: List[TypeDefinition], location: Location): TypeDefinition = {
    val message = "Method \"%s(%s)\" called on object of class \"%s\", but that method is not defined for that class."
        .format(methodName, parameterTypes.map(_.getName()).mkString(", "), className)

    val typeCheckError = CompilerMessage(CompilerError, TypeCheckingError, Some(location),
      message)

    typeCheckingErrors.append(typeCheckError)

    FailType
  }

  private def failUnknownOperatorTypes(operatorSymbol: String, firstType: TypeDefinition, secondType: TypeDefinition, operatorMatches: List[BinaryOperationDefinition], location: Location): TypeDefinition = {
    val messageStart = "Found instance of %s with parameters of type \"%s\" and \"%s\". No version of the operation was found for this type combination.\n\nValid type combinations for this operator are:\n\n".format(
      operatorSymbol,
      firstType.getName(), secondType.getName()
    )

    val validTypes = for (op <- operatorMatches)
      yield "\"%s\" and \"%s\"".format(
        op.firstParamType.getName(), op.secondParamType.getName())

    val message = messageStart + validTypes.mkString("\n")

    val typeCheckError = CompilerMessage(CompilerError, TypeCheckingError, Some(location),
      message)

    typeCheckingErrors.append(typeCheckError)

    FailType
  }

  private def failReturnType(actualType: TypeDefinition, expectedType: TypeDefinition, a: TypeVisitorContext): Unit = {
    val message = "Incorrect return type for method \"%s\" of class \"%s\".\n\nExpected: %s\nFound: %s"
      .format(a.curMethod.name, a.curClass.getName(), expectedType.getName(), actualType.getName())

    val typeCheckError = CompilerMessage(CompilerError, TypeCheckingError, None,
      message)

    typeCheckingErrors.append(typeCheckError)
  }
}
