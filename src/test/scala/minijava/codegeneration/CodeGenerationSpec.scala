package minijava.codegeneration

import java.io.FileOutputStream

import minijava.Main
import minijava.typechecking.{MainClassType, TypeChecking}
import org.scalatest._
import sys.process._


class CodeGenerationSpec extends FlatSpec with Matchers {
  "CodeGenerationVisitor" should "work on the HelloOne example" in {
    val input = Main.readFile("examples/HelloOne.minijava")

    val ast = Main.parseString(input)

    ast.isRight shouldBe true

    val typeCheckResult = TypeChecking.typeCheck(ast.right.get)

    typeCheckResult.isRight shouldBe true

    val typeTable = typeCheckResult.right.get

    val visitor = new CodeGenerationVisitor()

    val mainClassType = typeTable.get("HelloOne").get.asInstanceOf[MainClassType]

    visitor.visitMainClassType("HelloOne.minijava", mainClassType)

    writeClassFile(visitor, "HelloOne.class")

    testOutput("HelloOne", "1\n")
  }

  it should "work on the HelloTwo example" in {
    val input = Main.readFile("examples/HelloTwo.minijava")

    val ast = Main.parseString(input)

    ast.isRight shouldBe true

    val typeCheckResult = TypeChecking.typeCheck(ast.right.get)

    typeCheckResult.isRight shouldBe true

    val typeTable = typeCheckResult.right.get

    val visitor = new CodeGenerationVisitor()

    val mainClassType = typeTable.get("HelloTwo").get.asInstanceOf[MainClassType]

    visitor.visitMainClassType("HelloTwo.minijava", mainClassType)

    writeClassFile(visitor, "HelloTwo.class")

    testOutput("HelloTwo", "2\n")
  }

  it should "work on the HelloMinus example" in {
    val input = Main.readFile("examples/HelloMinus.minijava")

    val ast = Main.parseString(input)

    ast.isRight shouldBe true

    val typeCheckResult = TypeChecking.typeCheck(ast.right.get)

    typeCheckResult.isRight shouldBe true

    val typeTable = typeCheckResult.right.get

    val visitor = new CodeGenerationVisitor()

    val mainClassType = typeTable.get("HelloMinus").get.asInstanceOf[MainClassType]

    visitor.visitMainClassType("HelloMinus.minijava", mainClassType)

    writeClassFile(visitor, "HelloMinus.class")

    testOutput("HelloMinus", "2\n")
  }

  it should "work on the HelloTimes example" in {
    val input = Main.readFile("examples/HelloTimes.minijava")

    val ast = Main.parseString(input)

    ast.isRight shouldBe true

    val typeCheckResult = TypeChecking.typeCheck(ast.right.get)

    typeCheckResult.isRight shouldBe true

    val typeTable = typeCheckResult.right.get

    val visitor = new CodeGenerationVisitor()

    val mainClassType = typeTable.get("HelloTimes").get.asInstanceOf[MainClassType]

    visitor.visitMainClassType("HelloTimes.minijava", mainClassType)

    writeClassFile(visitor, "HelloTimes.class")

    testOutput("HelloTimes", "6\n")
  }

  it should "work on the IfTrue example" in {
    val input = Main.readFile("examples/IfTrue.minijava")

    val ast = Main.parseString(input)

    ast.isRight shouldBe true

    val typeCheckResult = TypeChecking.typeCheck(ast.right.get)

    typeCheckResult.isRight shouldBe true

    val typeTable = typeCheckResult.right.get

    val visitor = new CodeGenerationVisitor()

    val mainClassType = typeTable.get("IfTrue").get.asInstanceOf[MainClassType]

    visitor.visitMainClassType("IfTrue.minijava", mainClassType)

    writeClassFile(visitor, "IfTrue.class")

    testOutput("IfTrue", "1\n")
  }

  private def writeClassFile(visitor: CodeGenerationVisitor, classFileName: String): Unit = {
    val fos = new FileOutputStream(classFileName)
    try {
      fos.write(visitor.getClassWriter().toByteArray)
    }
    finally if (fos != null) fos.close()
  }

  private def testOutput(mainClassName: String, expectedOutput: String): Assertion = {
    val command = f"java $mainClassName"

    val actualOutput = command !!

    actualOutput shouldBe expectedOutput
  }
}
