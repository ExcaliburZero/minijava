package minijava.codegeneration

import minijava.grammar._
import minijava.typechecking._
import org.objectweb.asm.{ClassWriter, Label, MethodVisitor, Opcodes}

import scala.collection.mutable.ArrayBuffer

class CodeGenerationVisitor extends ASTVisitor[MethodVisitor, Unit] {
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
    val methodVariable = assignmentStatement.context.get.asInstanceOf[MethodVariable]

    assert(methodVariable.location == LocalVariable)

    val method = methodVariable.method

    val localVariableIndex = method.localVariables.map(_.name).indexOf(assignmentStatement.name.name)
    val index = 1 + method.parameters.length + localVariableIndex

    // Push the expression to be assigned
    visit(assignmentStatement.expression, a)

    // Store the pushed value into the correct variable
    a.visitIntInsn(Opcodes.ISTORE, index) // TODO: Change this to workm for all variable types
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
      case _ => ???
    }
  }

  override def visitMethodCallExpression(methodCallExpression: MethodCallExpression, a: MethodVisitor): Unit = {
    // Push the object to call the method on
    visit(methodCallExpression.objectExpression, a)

    // Push all of the parameters
    methodCallExpression.parameters.foreach(visit(_, a))

    // Call the method
    a.visitMethodInsn(
      Opcodes.INVOKEVIRTUAL,
      methodCallExpression.classType.get.getName(),
      methodCallExpression.methodName.name,
      methodCallExpression.method.get.getSignature()
    )
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
      /*case MethodVariable(method, Parameter) =>
        // TODO: Handle other types of variables
        val paramIndex = method.parameters.map(_.name).indexOf(identifierExpression.name.name)
        a.visitIntInsn(Opcodes.ILOAD, 1 + paramIndex)
      case MethodVariable(method, LocalVariable) =>
        val index = getMethodVariable()*/
      case methodVariable: MethodVariable =>
        val index = getMethodVariableIndex(methodVariable, identifierExpression.name.name)
        a.visitIntInsn(Opcodes.ILOAD, index)
      case _ => ???
    }
  }

  override def visitThisLiteral(a: MethodVisitor): Unit = {
    // Push the "this" object
    a.visitIntInsn(Opcodes.ALOAD, 0)
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
    val classWriter = new ClassWriter(true)

    classWriters.append((mainClassType.name, classWriter))

    // Create main class
    classWriter.visit(
      49,
      Opcodes.ACC_PUBLIC + Opcodes.ACC_SUPER,
      mainClassType.name,
      null,
      "java/lang/Object",
      null
    )

    classWriter.visitSource(fileName, null)

    // Constructor
    val constructorVisitor = classWriter.visitMethod(
      Opcodes.ACC_PUBLIC,
      "<init>",
      "()V",
      null,
      null
    )

    constructorVisitor.visitVarInsn(Opcodes.ALOAD, 0)
    constructorVisitor.visitMethodInsn(
      Opcodes.INVOKESPECIAL,
      "java/lang/Object",
      "<init>",
      "()V"
    )
    constructorVisitor.visitInsn(Opcodes.RETURN)
    constructorVisitor.visitMaxs(1, 1)
    constructorVisitor.visitEnd()

    // Main method
    visitMainMethod(classWriter, mainClassType.mainMethod)
  }

  private def visitMainMethod(classWriter: ClassWriter, mainMethod: Method): Unit = {
    val methodVisitor = classWriter.visitMethod(
      Opcodes.ACC_PUBLIC + Opcodes.ACC_STATIC,
      mainMethod.name,
      "([Ljava/lang/String;)V",
      null,
      null
    )

    assert(mainMethod.statements.length == 1)
    visit(mainMethod.statements.head, methodVisitor)

    methodVisitor.visitInsn(Opcodes.RETURN)

    methodVisitor.visitMaxs(2, 1) // TODO: do this better? Maybe have each visit return a max stack?

    methodVisitor.visitEnd()
  }

  def visitClassType(fileName: String, classType: ClassType): Unit = {
    val classWriter = new ClassWriter(true)

    classWriters.append((classType.name, classWriter))

    classWriter.visit(
      49,
      Opcodes.ACC_PUBLIC + Opcodes.ACC_SUPER,
      classType.name,
      null,
      "java/lang/Object", // TODO: This likely should be the parent class
      null
    )

    classWriter.visitSource(fileName, null)

    // Constructor
    val constructorVisitor = classWriter.visitMethod(
      Opcodes.ACC_PUBLIC,
      "<init>",
      "()V",
      null,
      null
    )

    constructorVisitor.visitVarInsn(Opcodes.ALOAD, 0)
    constructorVisitor.visitMethodInsn(
      Opcodes.INVOKESPECIAL,
      "java/lang/Object", // TODO: This should be replaced with the parent class
      "<init>",
      "()V"
    )
    constructorVisitor.visitInsn(Opcodes.RETURN)
    constructorVisitor.visitMaxs(1, 1)
    constructorVisitor.visitEnd()

    // TODO: Add class instance variables

    for (method <- classType.methods) visitMethod(classWriter, method)
  }

  private def visitMethod(classWriter: ClassWriter, method: Method): Unit = {
    // Create the method
    val methodVisitor = classWriter.visitMethod(
      Opcodes.ACC_PUBLIC,
      method.name,
      method.getSignature(),
      null,
      null
    )

    // Create labels to mark the beginning and end of the method in order to mark the scope of the local variables
    val methodStartLabel = new Label()
    val methodEndLabel = new Label()

    // Add the local variables
    for ((local, index) <- method.localVariables.zipWithIndex) {
      methodVisitor.visitLocalVariable(
        local.name,
        local.typeName match {
          case "int" => "I"
          case "boolean" => "Z"
          case _ => ???
        },
        null,
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
    method.returnType match {
      case "int" => methodVisitor.visitInsn(Opcodes.IRETURN)
      case "boolean" => methodVisitor.visitInsn(Opcodes.IRETURN)
      case "void" => methodVisitor.visitInsn(Opcodes.RETURN)
      case _ => ??? // TODO: Implement for the remaining types
    }

    // Add the label to mark the end of the method
    methodVisitor.visitLabel(methodEndLabel)

    // Specify the max stack size and number of local variables for the method
    methodVisitor.visitMaxs(2, method.localVariables.length) // TODO: do this better? Maybe have each visit return a max stack?

    methodVisitor.visitEnd()
  }

  private def getMethodVariableIndex(methodVariable: MethodVariable, variableName: String): Int = {
    methodVariable match {
      case MethodVariable(method, Parameter) =>
        val paramIndex = method.parameters.map(_.name).indexOf(variableName)

        1 + paramIndex
      case MethodVariable(method, LocalVariable) =>
        val localVariableIndex = method.localVariables.map(_.name).indexOf(variableName)

        1 + method.parameters.length + localVariableIndex
    }
  }
}
