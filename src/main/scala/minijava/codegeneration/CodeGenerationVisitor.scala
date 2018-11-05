package minijava.codegeneration

import minijava.grammar._
import minijava.typechecking.{MainClassType, Method}
import org.objectweb.asm.{ClassWriter, Label, MethodVisitor, Opcodes}

class CodeGenerationVisitor extends ASTVisitor[MethodVisitor, Unit] {
  private val classWriter = new ClassWriter(true)

  def getClassWriter(): ClassWriter = {
    classWriter
  }

  private def visitMainMethod(mainMethod: Method): Unit = {
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

        val falseLabel = new Label()
        val endLabel = new Label()

        // If both true, then jump to push false
        a.visitInsn(Opcodes.IADD)
        a.visitJumpInsn(Opcodes.IFNE, falseLabel)

        visitTrueLiteral(a)                     // Since both were true, push true
        a.visitJumpInsn(Opcodes.GOTO, endLabel) // Jump to skip past the push of false

        a.visitLabel(falseLabel)  // Add label for pushing false
        visitFalseLiteral(a)      // Since at least one was not true, push false

        a.visitLabel(endLabel)  // Add label for end of comparison
      case _ => ???
    }
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

  override def visitParenedExpression(parenedExpression: ParenedExpression, a: MethodVisitor): Unit = {
    visit(parenedExpression.expression, a)
  }

  def visitMainClassType(fileName: String, mainClassType: MainClassType): Unit = {
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
    visitMainMethod(mainClassType.mainMethod)
  }
}
