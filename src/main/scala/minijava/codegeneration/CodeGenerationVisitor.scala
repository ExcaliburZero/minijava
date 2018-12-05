package minijava.codegeneration

import minijava.grammar._
import minijava.typechecking._
import org.objectweb.asm.{ClassWriter, Label, MethodVisitor, Opcodes}

import scala.collection.mutable.ArrayBuffer

class CodeGenerationVisitor extends ASTVisitor[MethodVisitor, Unit] {
  private val INT_TYPE = 10

  private val NO_DEBUG_INFO: String = null
  private val NO_GENERICS: String = null
  private val NO_INTERFACES: Array[String] = null
  private val NO_INITIAL_VALUE: Object = null
  private val NO_EXCEPTIONS: Array[String] = null

  private val classWriters = new ArrayBuffer[(String, ClassWriter)]()

  def getClassWriters(): List[(String, ClassWriter)] = {
    classWriters.toList
  }

  override def visitStatementBlock(statementBlock: StatementBlock, a: MethodVisitor): Unit = {
    statementBlock.statements.foreach(visit(_, a))
  }

  override def visitIfStatement(ifStatement: IfStatement, a: MethodVisitor): Unit = {
    val elseLabel = new Label()
    val endLabel = new Label()

    // Add the bytecode to evaluate the condition and push the result to the stack
    visit(ifStatement.condition, a)

    // Jump to the else clause if the condition is false
    a.visitJumpInsn(Opcodes.IFNE, elseLabel)

    visit(ifStatement.thenClause, a)        // Add bytecode for if clause
    a.visitJumpInsn(Opcodes.GOTO, endLabel) // At the end of the if clause jump to skip the else

    a.visitLabel(elseLabel)                 // Add label for else clause
    visit(ifStatement.elseClause,a)         // Add bytecode for else clause

    // Add label for the end of the if, else statement
    a.visitLabel(endLabel)
  }

  override def visitWhileStatement(whileStatement: WhileStatement, a: MethodVisitor): Unit = {
    val conditionLabel = new Label()
    val bodyLabel = new Label()

    // Jump to check the condition
    a.visitJumpInsn(Opcodes.GOTO, conditionLabel)

    // Add the label and bytecode for the body of the loop
    a.visitLabel(bodyLabel)
    visit(whileStatement.statement, a)

    // Add the label and bytecode for the condition of the loop
    a.visitLabel(conditionLabel)
    visit(whileStatement.condition, a)
    a.visitJumpInsn(Opcodes.IFEQ, bodyLabel)
  }

  override def visitPrintStatement(printStatement: PrintStatement, a: MethodVisitor): Unit = {
    // Load up the static PrintStream so we can call println on it
    a.visitFieldInsn(
      Opcodes.GETSTATIC,
      "java/lang/System",
      "out",
      "Ljava/io/PrintStream;"
    )

    // Add all the bytecode to determine the integer to print
    visit(printStatement.expression, a)

    // Call the println method to print out the integer from the expression
    a.visitMethodInsn(
      Opcodes.INVOKEVIRTUAL,
      "java/io/PrintStream",
      "println",
      "(I)V"
    )
  }

  override def visitAssignmentStatement(assignmentStatement: AssignmentStatement, a: MethodVisitor): Unit = {
    assignmentStatement.context.get match {
      case methodVariable: MethodVariable =>
        // Get the variable info and index
        val (variable, index) = getMethodVariableWithIndex(methodVariable, assignmentStatement.name.name)

        // Push the expression to be assigned
        visit(assignmentStatement.expression, a)

        // Store the pushed value into the correct variable
        a.visitIntInsn(getStoreInsn(variable.typeName), index)

      case classType: ClassType =>
        val variableName = assignmentStatement.name.name
        val variableTypeName = classType.variables.filter(_.name == variableName).head.typeName

        // Load the "this" object to access the class variable to set its value later
        a.visitIntInsn(Opcodes.ALOAD, 0)

        // Push the value to be assigned
        visit(assignmentStatement.expression, a)

        // Store the value into the class variable
        a.visitFieldInsn(
          Opcodes.PUTFIELD,
          classType.name,
          variableName,
          TypeDescription.convertType(variableTypeName)
        )
    }
  }

  override def visitArrayAssignmentStatement(arrayAssignmentStatement: ArrayAssignmentStatement, a: MethodVisitor): Unit = {
    arrayAssignmentStatement.context.get match {
      case methodVariable: MethodVariable =>
        // Get the variable info and index
        val (variable, index) = getMethodVariableWithIndex(methodVariable, arrayAssignmentStatement.name.name)

        // Load the array to store the value in
        a.visitIntInsn(getLoadInsn(variable.typeName), index)

        // Push the array index to store the value in
        visit(arrayAssignmentStatement.indexExpression, a)

        // Push the value to store
        visit(arrayAssignmentStatement.valueExpression, a)

        // Store the value into the array
        a.visitInsn(Opcodes.IASTORE)
      case classType: ClassType =>
        val variableName = arrayAssignmentStatement.name.name
        val variableTypeName = classType.variables.filter(_.name == variableName).head.typeName

        // Load the "this" object to access the class variable to set its value later
        a.visitIntInsn(Opcodes.ALOAD, 0)

        // Load the variable from the "this" object
        a.visitFieldInsn(
          Opcodes.GETFIELD,
          classType.name,
          variableName,
          TypeDescription.convertType(variableTypeName)
        )

        // Push the array index to store the value in
        visit(arrayAssignmentStatement.indexExpression, a)

        // Push the value to store
        visit(arrayAssignmentStatement.valueExpression, a)

        // Store the value into the array
        a.visitInsn(Opcodes.IASTORE)
    }
  }

  override def visitBinaryOperationExpression(binaryOperationExpression: BinaryOperationExpression, a: MethodVisitor): Unit = {
    binaryOperationExpression.operator match {
      case Plus =>
        visit(binaryOperationExpression.firstExpression, a)
        visit(binaryOperationExpression.secondExpression, a)
        a.visitInsn(Opcodes.IADD)
      case Minus =>
        visit(binaryOperationExpression.firstExpression, a)
        visit(binaryOperationExpression.secondExpression, a)
        a.visitInsn(Opcodes.ISUB)
      case Times =>
        visit(binaryOperationExpression.firstExpression, a)
        visit(binaryOperationExpression.secondExpression, a)
        a.visitInsn(Opcodes.IMUL)
      case LessThan =>
        visit(binaryOperationExpression.firstExpression, a)
        visit(binaryOperationExpression.secondExpression, a)

        val falseLabel = new Label()
        val endLabel = new Label()

        // If not less than, then jump to push false
        a.visitJumpInsn(Opcodes.IF_ICMPGE, falseLabel)

        visitTrueLiteral(a)                     // Since it was less than, push true
        a.visitJumpInsn(Opcodes.GOTO, endLabel) // Jump to skip past the push of false

        a.visitLabel(falseLabel)  // Add label for pushing false
        visitFalseLiteral(a)      // Since it was not less than, push false

        a.visitLabel(endLabel)  // Add label for end of comparison
      case And =>
        visit(binaryOperationExpression.firstExpression, a)
        visit(binaryOperationExpression.secondExpression, a)
        a.visitInsn(Opcodes.IOR)  // Bitwise or is equivalent to and for true=0 and false=1
    }
  }

  override def visitArrayAccessExpression(arrayAccessExpression: ArrayAccessExpression, a: MethodVisitor): Unit = {
    // Push the array to access
    visit(arrayAccessExpression.arrayExpression, a)

    // Push the index to access
    visit(arrayAccessExpression.indexExpression, a)

    // Get the value out of the array
    a.visitInsn(Opcodes.IALOAD)
  }

  override def visitArrayLengthExpression(arrayLengthExpression: ArrayLengthExpression, a: MethodVisitor): Unit = {
    // Push the array to get the length of
    visit(arrayLengthExpression.arrayExpression, a)

    // Push the array length
    a.visitInsn(Opcodes.ARRAYLENGTH)
  }

  override def visitMethodCallExpression(methodCallExpression: MethodCallExpression, a: MethodVisitor): Unit = {
    // Create labels to handle the case where the object is null
    val nullLabel = new Label()
    val endNullCheckLabel = new Label()

    // Push the object to call the method on
    visit(methodCallExpression.objectExpression, a)

    // Check if the object is null
    a.visitInsn(Opcodes.DUP)
    a.visitJumpInsn(Opcodes.IFNULL, nullLabel)

    // Push all of the parameters
    methodCallExpression.parameters.foreach(visit(_, a))

    // Call the method
    a.visitMethodInsn(
      Opcodes.INVOKEVIRTUAL,
      methodCallExpression.classType.get.getName(),
      methodCallExpression.methodName.name,
      methodCallExpression.method.get.getSignature()
    )

    // Jump past the NullPointerException throw
    a.visitJumpInsn(Opcodes.GOTO, endNullCheckLabel)

    // Handle the case where the object is null
    a.visitLabel(nullLabel)

    // Pop the duplicated instance of the object
    a.visitInsn(Opcodes.POP)

    // Throw a NullPointerException
    visitThrowNullPointerException(a)

    // Add a label to end the null check
    a.visitLabel(endNullCheckLabel)
  }

  private def visitThrowNullPointerException(a: MethodVisitor): Unit = {
    // Create the NullPointerException object
    a.visitTypeInsn(Opcodes.NEW, "java/lang/NullPointerException")
    a.visitInsn(Opcodes.DUP)

    // Call its constructor
    a.visitMethodInsn(
      Opcodes.INVOKESPECIAL,
      "java/lang/NullPointerException",
      "<init>",
      "()V"
    )

    // Throw the exception
    a.visitInsn(Opcodes.ATHROW)
  }

  override def visitIntegerLiteral(integerLiteral: IntegerLiteral, a: MethodVisitor): Unit = {
    a.visitLdcInsn(integerLiteral.value)
  }

  override def visitTrueLiteral(a: MethodVisitor): Unit = {
    a.visitInsn(Opcodes.ICONST_0)
  }

  override def visitFalseLiteral(a: MethodVisitor): Unit = {
    a.visitInsn(Opcodes.ICONST_1)
  }

  override def visitIdentifierExpression(identifierExpression: IdentifierExpression, a: MethodVisitor): Unit = {
    identifierExpression.context.get match {
      case methodVariable: MethodVariable =>
        val index = getMethodVariableWithIndex(methodVariable, identifierExpression.name.name)._2
        val variableType = methodVariable.getTypeName(identifierExpression.name.name)

        // Push the value stored in the variable
        a.visitIntInsn(getLoadInsn(variableType), index)
      case classType: ClassType =>
        val variableName = identifierExpression.name.name
        val variableTypeName = classType.variables.filter(_.name == variableName).head.typeName

        // Load the "this" object to access the class variable
        a.visitIntInsn(Opcodes.ALOAD, 0)

        // Push the class variable value
        a.visitFieldInsn(
          Opcodes.GETFIELD,
          classType.name,
          variableName,
          TypeDescription.convertType(variableTypeName)
        )
    }
  }

  override def visitThisLiteral(a: MethodVisitor): Unit = {
    // Push the "this" object
    a.visitIntInsn(Opcodes.ALOAD, 0)
  }

  override def visitNewIntArrayExpression(intArrayExpression: NewIntArrayExpression, a: MethodVisitor): Unit = {
    // Push the value for the array length
    visit(intArrayExpression.lengthExpression, a)

    // Create the array
    a.visitIntInsn(Opcodes.NEWARRAY, INT_TYPE)
  }

  override def visitNewObjectExpression(newObjectExpression: NewObjectExpression, a: MethodVisitor): Unit = {
    // Create the new object instance
    a.visitTypeInsn(Opcodes.NEW, newObjectExpression.className.name)

    // Duplicate the object so that it will remain pushed after the constructor is called on it
    a.visitInsn(Opcodes.DUP)

    // Call the class' constructor
    a.visitMethodInsn(
      Opcodes.INVOKESPECIAL,
      newObjectExpression.className.name,
      "<init>",
      "()V"
    )
  }

  override def visitNegatedExpression(negatedExpression: NegatedExpression, a: MethodVisitor): Unit = {
    // Push the value to be negated
    visit(negatedExpression.expression, a)

    // XOR the boolean value with 1 to negate it, 0 => 1 and 1 => 0
    a.visitInsn(Opcodes.ICONST_1)
    a.visitInsn(Opcodes.IXOR)
  }

  override def visitParenedExpression(parenedExpression: ParenedExpression, a: MethodVisitor): Unit = {
    visit(parenedExpression.expression, a)
  }

  def visitMainClassType(fileName: String, mainClassType: MainClassType): Unit = {
    val classWriter = new ClassWriter(0)

    classWriters.append((mainClassType.name, classWriter))

    // Create main class
    val parentClass = mainClassType.getParentClass().getOrElse("java/lang/Object")
    classWriter.visit(
      49,
      Opcodes.ACC_PUBLIC + Opcodes.ACC_SUPER,
      mainClassType.name,
      NO_GENERICS,
      parentClass,
      NO_INTERFACES
    )

    classWriter.visitSource(fileName, NO_DEBUG_INFO)

    // Add class constructor
    visitConstructor(classWriter, parentClass)

    // Main method
    visitMainMethod(classWriter, mainClassType.mainMethod)
  }

  private def visitMainMethod(classWriter: ClassWriter, mainMethod: Method): Unit = {
    // Create the main method
    val methodVisitor = classWriter.visitMethod(
      Opcodes.ACC_PUBLIC + Opcodes.ACC_STATIC,
      mainMethod.name,
      "([Ljava/lang/String;)V",
      NO_GENERICS,
      NO_EXCEPTIONS
    )

    // Double check that there is only one parameter, as a sanity check
    assert(mainMethod.statements.length == 1)

    // Add the bytecode for the statement
    visit(mainMethod.statements.head, methodVisitor)

    // Return and end the method
    methodVisitor.visitInsn(Opcodes.RETURN)

    methodVisitor.visitMaxs(2, 1) // TODO: do this better? Maybe have each visit return a max stack?

    methodVisitor.visitEnd()
  }

  def visitClassType(fileName: String, classType: ClassType): Unit = {
    val classWriter = new ClassWriter(0)

    classWriters.append((classType.name, classWriter))

    // Create the class
    val parentClass = classType.getParentClass().getOrElse("java/lang/Object")
    classWriter.visit(
      49,
      Opcodes.ACC_PUBLIC + Opcodes.ACC_SUPER,
      classType.name,
      NO_GENERICS,
      parentClass,
      NO_INTERFACES
    )

    classWriter.visitSource(fileName, NO_DEBUG_INFO)

    // Add class constructor
    visitConstructor(classWriter, parentClass)

    // Add class instance variables
    for (variable <- classType.variables) classWriter.visitField(
      Opcodes.ACC_PUBLIC,
      variable.name,
      TypeDescription.convertType(variable.typeName),
      NO_GENERICS,
      NO_INITIAL_VALUE
    )

    // Add class methods
    for (method <- classType.methods) visitMethod(classWriter, method)
  }

  private def visitConstructor(classWriter: ClassWriter, parentClass: String): Unit = {
    // Create the constructor method
    val constructorVisitor = classWriter.visitMethod(
      Opcodes.ACC_PUBLIC,
      "<init>",
      "()V",
      NO_GENERICS,
      NO_EXCEPTIONS
    )

    // Load the "this" object and call the parent constructor
    constructorVisitor.visitVarInsn(Opcodes.ALOAD, 0)
    constructorVisitor.visitMethodInsn(
      Opcodes.INVOKESPECIAL,
      parentClass,
      "<init>",
      "()V"
    )

    // Return and end the method
    constructorVisitor.visitInsn(Opcodes.RETURN)
    constructorVisitor.visitMaxs(1, 1)
    constructorVisitor.visitEnd()
  }

  private def visitMethod(classWriter: ClassWriter, method: Method): Unit = {
    // Create the method
    val methodVisitor = classWriter.visitMethod(
      Opcodes.ACC_PUBLIC,
      method.name,
      method.getSignature(),
      NO_GENERICS,
      NO_EXCEPTIONS
    )

    // Create labels to mark the beginning and end of the method in order to mark the scope of the local variables
    val methodStartLabel = new Label()
    val methodEndLabel = new Label()

    // Add the local variables
    for ((local, index) <- method.localVariables.zipWithIndex) {
      val typeDescriptor = TypeDescription.convertType(local.typeName)

      methodVisitor.visitLocalVariable(
        local.name,
        typeDescriptor,
        NO_GENERICS,
        methodStartLabel,
        methodEndLabel,
        index + method.parameters.length + 1
      )
    }

    // Add the label to mark the start of the method
    methodVisitor.visitLabel(methodStartLabel)

    // Add the bytecode for the statements and the return expression
    for (statement <- method.statements) visit(statement, methodVisitor)
    visit(method.returnExpression.get, methodVisitor)

    // Add a return bytecode for the correct return type
    methodVisitor.visitInsn(getReturnInsn(method.returnType))

    // Add the label to mark the end of the method
    methodVisitor.visitLabel(methodEndLabel)

    // Specify the max stack size and number of local variables for the method
    methodVisitor.visitMaxs(2, method.localVariables.length) // TODO: do this better? Maybe have each visit return a max stack?

    methodVisitor.visitEnd()
  }

  private def getMethodVariableWithIndex(methodVariable: MethodVariable, variableName: String): (Variable, Int) = {
    methodVariable match {
      case MethodVariable(method, Parameter) =>
        val paramIndex = method.parameters.map(_.name).indexOf(variableName)

        (method.parameters(paramIndex), 1 + paramIndex)
      case MethodVariable(method, LocalVariable) =>
        val localVariableIndex = method.localVariables.map(_.name).indexOf(variableName)

        (method.localVariables(localVariableIndex), 1 + method.parameters.length + localVariableIndex)
    }
  }

  private def getLoadInsn(typeName: String): Int = {
    typeName match {
      case "int" | "boolean" => Opcodes.ILOAD
      case "void" => ???
      case "int[]" => Opcodes.ALOAD
      case _ => Opcodes.ALOAD
    }
  }

  private def getStoreInsn(typeName: String): Int = {
    typeName match {
      case "int" | "boolean" => Opcodes.ISTORE
      case "void" => ???
      case "int[]" => Opcodes.ASTORE
      case _ => Opcodes.ASTORE
    }
  }

  private def getReturnInsn(typeName: String): Int = {
    typeName match {
      case "int" | "boolean" => Opcodes.IRETURN
      case "void" => ???
      case "int[]" => Opcodes.ARETURN
      case _ => Opcodes.ARETURN
    }
  }
}
