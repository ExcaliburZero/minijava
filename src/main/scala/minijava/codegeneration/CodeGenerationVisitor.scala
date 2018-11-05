package minijava.codegeneration

import minijava.grammar.ASTVisitor
import minijava.typechecking.MainClassType
import org.objectweb.asm.{ClassWriter, Opcodes}

class CodeGenerationVisitor extends ASTVisitor[Unit, Unit] {
  private val classWriter = new ClassWriter(true)

  def getClassWriter(): ClassWriter = {
    classWriter
  }

  def visitMainClassType(fileName: String, mainClassType: MainClassType): Unit = {
    //val mainClassId = classWriter.newClass(mainClassType.name)

    classWriter.visit(
      49,
      Opcodes.ACC_PUBLIC + Opcodes.ACC_SUPER,
      mainClassType.name,
      null,
      "java/lang/Object",
      null
    )

    classWriter.visitSource(fileName, null)

    // constructor

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

    // main method

    val mainMethod = mainClassType.mainMethod

    val methodVisitor = classWriter.visitMethod(
      Opcodes.ACC_PUBLIC + Opcodes.ACC_STATIC,
      mainMethod.name,
      "([Ljava/lang/String;)V",
      null,
      null
    )

    methodVisitor.visitFieldInsn(
      Opcodes.GETSTATIC,
      "java/lang/System",
      "out",
      "Ljava/io/PrintStream;"
    )

    methodVisitor.visitLdcInsn(0)

    methodVisitor.visitMethodInsn(
      Opcodes.INVOKEVIRTUAL,
      "java/io/PrintStream",
      "println",
      "(I)V"
    )

    methodVisitor.visitInsn(Opcodes.RETURN)

    methodVisitor.visitMaxs(2, 1)

    methodVisitor.visitEnd()
  }
}
