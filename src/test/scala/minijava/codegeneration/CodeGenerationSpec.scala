package minijava.codegeneration

import java.io.FileOutputStream

import minijava.Main
import minijava.typechecking.{ClassType, MainClassType, TypeChecking}
import org.scalatest._

import sys.process._


class CodeGenerationSpec extends FunSpec with Matchers  {
  describe("CodeGenerationVisitor") {
    describe("should work with the minijava examples") {
      it("Factorial") {
        testProgram("examples/", "Factorial.minijava",
          "Factorial",
          "3628800\n"
        )
      }

      it("BinarySearch") {
        testProgram("examples/", "BinarySearch.minijava",
          "BinarySearch",
          "20\n21\n22\n23\n24\n25\n26\n27\n28\n29\n30\n31\n32\n33\n34\n35\n36\n37\n38\n99999\n0\n0\n1\n1\n1\n1\n0\n0\n999\n"
        )
      }

      it("BubbleSort") {
        testProgram("examples/", "BubbleSort.minijava",
          "BubbleSort",
          "20\n7\n12\n18\n2\n11\n6\n9\n19\n5\n99999\n2\n5\n6\n7\n9\n11\n12\n18\n19\n20\n0\n"
        )
      }

      it("TreeVisitor") {
        testProgram("examples/", "TreeVisitor.minijava",
          "TreeVisitor",
          "16\n100000000\n4\n8\n12\n14\n16\n20\n24\n28\n100000000\n50000000\n333\n333\n333\n28\n24\n333\n20\n16\n333\n333\n333\n14\n12\n8\n333\n4\n100000000\n1\n1\n1\n0\n1\n4\n8\n14\n16\n20\n24\n28\n0\n0\n"
        )
      }

      it("QuickSort") {
        testProgram("examples/", "QuickSort.minijava",
          "QuickSort",
          "20\n7\n12\n18\n2\n11\n6\n9\n19\n5\n9999\n2\n5\n6\n7\n9\n11\n12\n18\n19\n20\n0\n"
        )
      }

      it("LinearSearch") {
        testProgram("examples/", "LinearSearch.minijava",
          "LinearSearch",
          "10\n11\n12\n13\n14\n15\n16\n17\n18\n9999\n0\n1\n1\n0\n55\n"
        )
      }

      it("LinkedList") {
        testProgram("examples/", "LinkedList.minijava",
          "LinkedList",
          "25\n10000000\n39\n25\n10000000\n22\n39\n25\n1\n0\n10000000\n28\n22\n39\n25\n2220000\n-555\n-555\n28\n22\n25\n33300000\n22\n25\n44440000\n0\n"
        )
      }

      it("BinaryTree") {
        testProgram("examples/", "BinaryTree.minijava",
          "BinaryTree",
          "16\n100000000\n8\n16\n4\n8\n12\n14\n16\n20\n24\n28\n1\n1\n1\n0\n1\n4\n8\n14\n16\n20\n24\n28\n0\n0\n"
        )
      }
    }

    describe("print statements") {
      it("should print a number literal") {
        testProgram("examples/", "HelloOne.minijava",
          "HelloOne",
          "1\n"
        )
      }
    }

    describe("int") {
      it("should work with addition of int literals") {
        testProgram("examples/", "HelloTwo.minijava",
          "HelloTwo",
          "2\n"
        )
      }

      it("should work with subtraction of int literals") {
        testProgram("examples/", "HelloMinus.minijava",
          "HelloMinus",
          "2\n"
        )
      }

      it("should work with multiplication of int literals") {
        testProgram("examples/", "HelloTimes.minijava",
          "HelloTimes",
          "6\n"
        )
      }
    }

    describe("if statements") {
      it("should run the then clause on a true boolean") {
        testProgram("examples/", "IfTrue.minijava",
          "IfTrue",
          "1\n"
        )
      }

      it("should run the else clause on a false boolean") {
        testProgram("examples/", "IfFalse.minijava",
          "IfFalse",
          "2\n"
        )
      }

      it("should run the then clause on a true less than condition") {
        testProgram("examples/", "IfLessThanTrue.minijava",
          "IfLessThanTrue",
          "1\n"
        )
      }

      it("should run the else clause on a false less than condition") {
        testProgram("examples/", "IfLessThanFalse.minijava",
          "IfLessThanFalse",
          "2\n"
        )
      }

      it("should run the else clause on an equal (false) less than condition") {
        testProgram("examples/", "IfLessThanFalseEqual.minijava",
          "IfLessThanFalseEqual",
          "2\n"
        )
      }

      it("should run the then clause on the and of two true booleans") {
        testProgram("examples/", "IfAndTrueTrue.minijava",
          "IfAndTrueTrue",
          "1\n"
        )
      }

      it("should run the else clause on the and of true and false") {
        testProgram("examples/", "IfAndTrueFalse.minijava",
          "IfAndTrueFalse",
          "2\n"
        )
      }

      it("should run the else clause on the and of false and true") {
        testProgram("examples/", "IfAndFalseTrue.minijava",
          "IfAndFalseTrue",
          "2\n"
        )
      }

      it("should run the else clause on the and of two false booleans") {
        testProgram("examples/", "IfAndFalseFalse.minijava",
          "IfAndFalseFalse",
          "2\n"
        )
      }

      it("should run the correct statements in a case with multiple consecutive if statements") {
        testProgram("examples/", "MultipleIfs.minijava",
          "MultipleIfs",
          "1\n3\n5\n"
        )
      }

      it("should run the correct statements in a case with nested if statements") {
        testProgram("examples/", "NestedIfs.minijava",
          "NestedIfs",
          "2\n4\n"
        )
      }

      it("should run the else clause on the negation of a true boolean") {
        testProgram("examples/", "IfNotTrue.minijava",
          "IfNotTrue",
          "2\n"
        )
      }

      it("should run the then clause on the negation of a false boolean") {
        testProgram("examples/", "IfNotFalse.minijava",
          "IfNotFalse",
          "1\n"
        )
      }
    }

    describe("variables") {
      it("should allow a method parameter (pass, read)") {
        testProgram("examples/", "MethodParameter.minijava",
          "MethodParameter",
          "7\n"
        )
      }

      it("should allow multiple method parameters (pass, read)") {
        testProgram("examples/", "MethodMultipleParameters.minijava",
          "MethodMultipleParameters",
          "-1\n"
        )
      }

      it("should allow local variables (define, set, read)") {
        testProgram("examples/", "LocalVariable.minijava",
          "LocalVariable",
          "1\n3\n"
        )
      }

      it("should allow class instance variables (define, set, read)") {
        testProgram("examples/", "ClassVariable.minijava",
          "ClassVariable",
          "10\n"
        )
      }

      it("should allow boolean variables") {
        testProgram("examples/", "BooleanVariables.minijava",
          "BooleanVariables",
          "2\n"
        )
      }

      it("should allow variables of defined class types") {
        testProgram("examples/", "ObjectVariables.minijava",
          "ObjectVariables",
          "2\n"
        )
      }
    }

    describe("int arrays") {
      it("should allow int arrays as local variables (declare, set, read)") {
        testProgram("examples/", "IntArray.minijava",
          "IntArray",
          "20\n"
        )
      }

      it("should allow int arrays as class instance variables (declare, set, read)") {
        testProgram("examples/", "IntArrayClassVariable.minijava",
          "IntArrayClassVariable",
          "9\n"
        )
      }

      it("should allow int arrays to be returned from methods") {
        testProgram("examples/", "IntArrayReturn.minijava",
          "IntArrayReturn",
          "3\n"
        )
      }

      it("should allow you to get the length of an int array") {
        testProgram("examples/", "IntArrayLength.minijava",
          "IntArrayLength",
          "8\n"
        )
      }

      it("should allow an int array to be created with a runtime-determined length") {
        testProgram("examples/", "IntArrayRuntimeLength.minijava",
          "IntArrayRuntimeLength",
          "20\n"
        )
      }
    }

    describe("method overriding") {
      it("should call the correct method on a child class") {
        testProgram("examples/", "Override.minijava",
          "Override",
          "1\n2\n0\n"
        )
      }

      it("should call the correct method on a child class in an inheritance chain") {
        testProgram("examples/", "MultipleOverride.minijava",
          "MultipleOverride",
          "1\n2\n3\n0\n"
        )
      }

      it("should call the correct method in a case of covariant override") {
        testProgram("examples/", "CovariantOverride.minijava",
          "Factorial",
          "1\n2\n"
        )
      }

      it("should call the correct method in a case of covariant override followed by a polymorhpic call") {
        testProgram("examples/", "CovariantPolymorphic.minijava",
          "CovariantPolymorphic",
          "1\n2\n3\n"
        )
      }
    }

    describe("miscellaneous") {
      it("should allow method calls") {
        testProgram("examples/", "MethodCall.minijava",
          "MethodCall",
          "2\n"
        )
      }

      it("should allow while loops that run at least once") {
        testProgram("examples/", "WhileLoop.minijava",
          "WhileLoop",
          "5\n"
        )
      }

      it("should allow while loops that are skipped") {
        testProgram("examples/", "WhileLoopSkip.minijava",
          "WhileLoopSkip",
          "0\n"
        )
      }

      it("should allow you to return an object of a class type") {
        testProgram("examples/", "ReturnObject.minijava",
          "ReturnObject",
          "2\n"
        )
      }

      it("should allow a method with 8 parameters to be called") {
        testProgram("examples/", "ManyParameters.minijava",
          "ManyParameters",
          "201600\n"
        )
      }
    }
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

    typeCheckResult._2.isEmpty shouldBe true

    val typeTable = typeCheckResult._1.get

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
