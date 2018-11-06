package minijava.codegeneration

import java.io.FileOutputStream

import minijava.Main
import minijava.typechecking.{ClassType, MainClassType, TypeChecking}
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

    writeClassFiles(visitor)

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

    writeClassFiles(visitor)

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

    writeClassFiles(visitor)

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

    writeClassFiles(visitor)

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

    writeClassFiles(visitor)

    testOutput("IfTrue", "1\n")
  }

  it should "work on the IfFalse example" in {
    val input = Main.readFile("examples/IfFalse.minijava")

    val ast = Main.parseString(input)

    ast.isRight shouldBe true

    val typeCheckResult = TypeChecking.typeCheck(ast.right.get)

    typeCheckResult.isRight shouldBe true

    val typeTable = typeCheckResult.right.get

    val visitor = new CodeGenerationVisitor()

    val mainClassType = typeTable.get("IfFalse").get.asInstanceOf[MainClassType]

    visitor.visitMainClassType("IfFalse.minijava", mainClassType)

    writeClassFiles(visitor)

    testOutput("IfFalse", "2\n")
  }

  it should "work on the IfLessThanTrue example" in {
    val input = Main.readFile("examples/IfLessThanTrue.minijava")

    val ast = Main.parseString(input)

    ast.isRight shouldBe true

    val typeCheckResult = TypeChecking.typeCheck(ast.right.get)

    typeCheckResult.isRight shouldBe true

    val typeTable = typeCheckResult.right.get

    val visitor = new CodeGenerationVisitor()

    val mainClassType = typeTable.get("IfLessThanTrue").get.asInstanceOf[MainClassType]

    visitor.visitMainClassType("IfLessThanTrue.minijava", mainClassType)

    writeClassFiles(visitor)

    testOutput("IfLessThanTrue", "1\n")
  }

  it should "work on the IfLessThanFalse example" in {
    val input = Main.readFile("examples/IfLessThanFalse.minijava")

    val ast = Main.parseString(input)

    ast.isRight shouldBe true

    val typeCheckResult = TypeChecking.typeCheck(ast.right.get)

    typeCheckResult.isRight shouldBe true

    val typeTable = typeCheckResult.right.get

    val visitor = new CodeGenerationVisitor()

    val mainClassType = typeTable.get("IfLessThanFalse").get.asInstanceOf[MainClassType]

    visitor.visitMainClassType("IfLessThanFalse.minijava", mainClassType)

    writeClassFiles(visitor)

    testOutput("IfLessThanFalse", "2\n")
  }

  it should "work on the IfLessThanFalseEqual example" in {
    val input = Main.readFile("examples/IfLessThanFalseEqual.minijava")

    val ast = Main.parseString(input)

    ast.isRight shouldBe true

    val typeCheckResult = TypeChecking.typeCheck(ast.right.get)

    typeCheckResult.isRight shouldBe true

    val typeTable = typeCheckResult.right.get

    val visitor = new CodeGenerationVisitor()

    val mainClassType = typeTable.get("IfLessThanFalseEqual").get.asInstanceOf[MainClassType]

    visitor.visitMainClassType("IfLessThanFalseEqual.minijava", mainClassType)

    writeClassFiles(visitor)

    testOutput("IfLessThanFalseEqual", "2\n")
  }

  it should "work on the IfAndTrueTrue example" in {
    testProgram("examples/", "IfAndTrueTrue.minijava",
      "IfAndTrueTrue",
      "1\n"
    )
  }

  it should "work on the IfAndTrueFalse example" in {
    testProgram("examples/", "IfAndTrueFalse.minijava",
      "IfAndTrueFalse",
      "2\n"
    )
  }

  it should "work on the IfAndFalseTrue example" in {
    testProgram("examples/", "IfAndFalseTrue.minijava",
      "IfAndFalseTrue",
      "2\n"
    )
  }

  it should "work on the IfAndFalseFalse example" in {
    testProgram("examples/", "IfAndFalseFalse.minijava",
      "IfAndFalseFalse",
      "2\n"
    )
  }

  it should "work on the MultipleIfs example" in {
    testProgram("examples/", "MultipleIfs.minijava",
      "MultipleIfs",
      "1\n3\n5\n"
    )
  }

  it should "work on the NestedIfs example" in {
    testProgram("examples/", "NestedIfs.minijava",
      "NestedIfs",
      "2\n4\n"
    )
  }

  it should "work on the IfNotTrue example" in {
    testProgram("examples/", "IfNotTrue.minijava",
      "IfNotTrue",
      "2\n"
    )
  }

  it should "work on the IfNotFalse example" in {
    testProgram("examples/", "IfNotFalse.minijava",
      "IfNotFalse",
      "1\n"
    )
  }

  it should "work on the MethodCall example" in {
    testProgram("examples/", "MethodCall.minijava",
      "MethodCall",
      "2\n"
    )
  }

  it should "work on the MethodParameter example" in {
    testProgram("examples/", "MethodParameter.minijava",
      "MethodParameter",
      "7\n"
    )
  }

  it should "work on the MethodMultipleParameters example" in {
    testProgram("examples/", "MethodMultipleParameters.minijava",
      "MethodMultipleParameters",
      "-1\n"
    )
  }

  it should "work on the LocalVariable example" in {
    testProgram("examples/", "LocalVariable.minijava",
      "LocalVariable",
      "1\n3\n"
    )
  }

  it should "work on the Factorial example" in {
    testProgram("examples/", "Factorial.minijava",
      "Factorial",
      "3628800\n"
    )
  }

  it should "work on the WhileLoop example" in {
    testProgram("examples/", "WhileLoop.minijava",
      "WhileLoop",
      "5\n"
    )
  }

  it should "work on the BooleanVariables example" in {
    testProgram("examples/", "BooleanVariables.minijava",
      "BooleanVariables",
      "2\n"
    )
  }

  it should "work on the ObjectVariables example" in {
    testProgram("examples/", "ObjectVariables.minijava",
      "ObjectVariables",
      "2\n"
    )
  }

  it should "work on the ReturnObject example" in {
    testProgram("examples/", "ReturnObject.minijava",
      "ReturnObject",
      "2\n"
    )
  }

  it should "work on the ClassVariable example" in {
    testProgram("examples/", "ClassVariable.minijava",
      "ClassVariable",
      "10\n"
    )
  }

  /*it should "work on the LinkedList example" in {
    testProgram("examples/", "LinkedList.minijava",
      "LinkedList",
      "2\n"
    )
  }*/

  private def writeClassFiles(visitor: CodeGenerationVisitor): Unit = {
    for ((classFileName, classWriter) <- visitor.getClassWriters()) {
      val fos = new FileOutputStream(f"$classFileName.class")
      try {
        fos.write(classWriter.toByteArray)
      }
      finally if (fos != null) fos.close()
    }
  }

  private def testProgram(filePath: String, fileName: String, mainClassName: String, expectedOutput: String): Assertion = {
    val input = Main.readFile(filePath + fileName)

    val ast = Main.parseString(input)

    ast.isRight shouldBe true

    val typeCheckResult = TypeChecking.typeCheck(ast.right.get)

    typeCheckResult.isRight shouldBe true

    val typeTable = typeCheckResult.right.get

    val visitor = new CodeGenerationVisitor()

    val mainClassType = typeTable.get(mainClassName).get.asInstanceOf[MainClassType]
    visitor.visitMainClassType(fileName, mainClassType)

    typeTable.types()
      .filter(_.isInstanceOf[ClassType])
      .map(_.asInstanceOf[ClassType])
      .foreach(visitor.visitClassType(fileName, _))

    writeClassFiles(visitor)

    testOutput(mainClassName, expectedOutput)
  }

  private def testOutput(mainClassName: String, expectedOutput: String): Assertion = {
    val command = f"java $mainClassName"

    val actualOutput = command !!

    actualOutput shouldBe expectedOutput
  }
}
