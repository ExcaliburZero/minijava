package minijava.codegeneration

import minijava.grammar._
import minijava.typechecking.{MainClassType, Method}
import org.objectweb.asm.{ClassWriter, MethodVisitor, Opcodes}

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
      case _ => ???
    }
  }

  override def visitIntegerLiteral(integerLiteral: IntegerLiteral, a: MethodVisitor): Unit = {
    a.visitLdcInsn(integerLiteral.value)
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
