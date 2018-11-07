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

  it should "work on the LinkedList example" in {
    testProgram("examples/", "LinkedList.minijava",
      "LinkedList",
      "25\n10000000\n39\n25\n10000000\n22\n39\n25\n1\n0\n10000000\n28\n22\n39\n25\n2220000\n-555\n-555\n28\n22\n25\n33300000\n22\n25\n44440000\n0\n"
    )
  }

  it should "work on the BinaryTree example" in {
    testProgram("examples/", "BinaryTree.minijava",
      "BinaryTree",
      "16\n100000000\n8\n16\n4\n8\n12\n14\n16\n20\n24\n28\n1\n1\n1\n0\n1\n4\n8\n14\n16\n20\n24\n28\n0\n0\n"
    )
  }

  it should "work on the IntArray example" in {
    testProgram("examples/", "IntArray.minijava",
      "IntArray",
      "20\n"
    )
  }

  it should "work on the IntArrayClassVariable example" in {
    testProgram("examples/", "IntArrayClassVariable.minijava",
      "IntArrayClassVariable",
      "9\n"
    )
  }

  it should "work on the IntArrayReturn example" in {
    testProgram("examples/", "IntArrayReturn.minijava",
      "IntArrayReturn",
      "3\n"
    )
  }

  it should "work on the IntArrayLength example" in {
    testProgram("examples/", "IntArrayLength.minijava",
      "IntArrayLength",
      "8\n"
    )
  }

  it should "work on the BinarySearch example" in {
    testProgram("examples/", "BinarySearch.minijava",
      "BinarySearch",
      "20\n21\n22\n23\n24\n25\n26\n27\n28\n29\n30\n31\n32\n33\n34\n35\n36\n37\n38\n99999\n0\n0\n1\n1\n1\n1\n0\n0\n999\n"
    )
  }

  it should "work on the BubbleSort example" in {
    testProgram("examples/", "BubbleSort.minijava",
      "BubbleSort",
      "20\n7\n12\n18\n2\n11\n6\n9\n19\n5\n99999\n2\n5\n6\n7\n9\n11\n12\n18\n19\n20\n0\n"
    )
  }

  it should "work on the QuickSort example" in {
    testProgram("examples/", "QuickSort.minijava",
      "QuickSort",
      "20\n7\n12\n18\n2\n11\n6\n9\n19\n5\n9999\n2\n5\n6\n7\n9\n11\n12\n18\n19\n20\n0\n"
    )
  }

  it should "work on the LinearSearch example" in {
    testProgram("examples/", "LinearSearch.minijava",
      "LinearSearch",
      "10\n11\n12\n13\n14\n15\n16\n17\n18\n9999\n0\n1\n1\n0\n55\n"
    )
  }

  it should "work on the TreeVisitor example" in {
    testProgram("examples/", "TreeVisitor.minijava",
      "TreeVisitor",
      "16\n100000000\n4\n8\n12\n14\n16\n20\n24\n28\n100000000\n50000000\n333\n333\n333\n28\n24\n333\n20\n16\n333\n333\n333\n14\n12\n8\n333\n4\n100000000\n1\n1\n1\n0\n1\n4\n8\n14\n16\n20\n24\n28\n0\n0\n"
    )
  }

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
