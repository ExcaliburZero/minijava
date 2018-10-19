package minijava.typechecking

import minijava.Main
import minijava.messages._
import org.scalatest._

class TypeCheckingSpec extends FlatSpec with Matchers {
  "typeCheck" should "fail when multiple classes are declared with the same name" in {
    val input = Main.readFile("examples/ClassNameDuplicate.minijava")

    val ast = Main.parseString(input)

    ast.isRight shouldBe true

    val typeCheckResult = TypeChecking.typeCheck(ast.right.get)

    typeCheckResult.isLeft shouldBe true

    val errors = typeCheckResult.left.get

    val expected = List(
      CompilerMessage(CompilerError, TypeCheckingError, Some(LineNumber(20)), "Duplicate declaration of class \"Fac\""),
      CompilerMessage(CompilerError, TypeCheckingError, Some(LineNumber(33)), "Duplicate declaration of class \"Fac\"")
    )

    errors shouldBe expected
  }

  it should "fail when a class is declared with the same name as the main class" in {
    val input = Main.readFile("examples/ClassDuplicateOfMain.minijava")

    val ast = Main.parseString(input)

    ast.isRight shouldBe true

    val typeCheckResult = TypeChecking.typeCheck(ast.right.get)

    typeCheckResult.isLeft shouldBe true

    val errors = typeCheckResult.left.get

    val expected = List(
      CompilerMessage(CompilerError, TypeCheckingError, Some(LineNumber(7)), "Duplicate declaration of class \"Factorial\"")
    )

    errors shouldBe expected
  }

  it should "fail when a class is declared with the same name as the FAIL type" in {
    val input = Main.readFile("examples/FAILClass.minijava")

    val ast = Main.parseString(input)

    ast.isRight shouldBe true

    val typeCheckResult = TypeChecking.typeCheck(ast.right.get)

    typeCheckResult.isLeft shouldBe true

    val errors = typeCheckResult.left.get

    val expected = List(
      CompilerMessage(CompilerError, TypeCheckingError, Some(LineNumber(20)), "Attempt to create class with reserved name \"FAIL\"")
    )

    errors shouldBe expected
  }

  it should "fail when the main class is declared with the same name as the FAIL type" in {
    val input = Main.readFile("examples/FAILMain.minijava")

    val ast = Main.parseString(input)

    ast.isRight shouldBe true

    val typeCheckResult = TypeChecking.typeCheck(ast.right.get)

    typeCheckResult.isLeft shouldBe true

    val errors = typeCheckResult.left.get

    val expected = List(
      CompilerMessage(CompilerError, TypeCheckingError, Some(LineNumber(1)), "Attempt to create class with reserved name \"FAIL\"")
    )

    errors shouldBe expected
  }

  it should "fail when a class variable is declared with the reserved FAIL type" in {
    val input = Main.readFile("examples/FAILClassVariable.minijava")

    val ast = Main.parseString(input)

    ast.isRight shouldBe true

    val typeCheckResult = TypeChecking.typeCheck(ast.right.get)

    typeCheckResult.isLeft shouldBe true

    val errors = typeCheckResult.left.get

    val expected = List(
      CompilerMessage(CompilerError, TypeCheckingError, Some(LineNumber(9)), "Attempt to create a class instance variable \"sal\" with reserved type \"FAIL\"")
    )

    errors shouldBe expected
  }

  it should "fail when a local variable is declared with the reserved FAIL type" in {
    val input = Main.readFile("examples/FAILVariable.minijava")

    val ast = Main.parseString(input)

    ast.isRight shouldBe true

    val typeCheckResult = TypeChecking.typeCheck(ast.right.get)

    typeCheckResult.isLeft shouldBe true

    val errors = typeCheckResult.left.get

    val expected = List(
      CompilerMessage(CompilerError, TypeCheckingError, Some(LineNumber(10)), "Attempt to create a local variable \"bad\" with reserved type \"FAIL\"")
    )

    errors shouldBe expected
  }

  it should "fail when a parameter is declared with the reserved FAIL type" in {
    val input = Main.readFile("examples/FAILParameter.minijava")

    val ast = Main.parseString(input)

    ast.isRight shouldBe true

    val typeCheckResult = TypeChecking.typeCheck(ast.right.get)

    typeCheckResult.isLeft shouldBe true

    val errors = typeCheckResult.left.get

    val expected = List(
      CompilerMessage(CompilerError, TypeCheckingError, Some(LineNumber(9)), "Attempt to create a parameter \"jane\" with reserved type \"FAIL\"")
    )

    errors shouldBe expected
  }

  it should "report multiple type checking errors when multiple are present" in {
    val input = Main.readFile("examples/MultTypeErrors.minijava")

    val ast = Main.parseString(input)

    ast.isRight shouldBe true

    val typeCheckResult = TypeChecking.typeCheck(ast.right.get)

    typeCheckResult.isLeft shouldBe true

    val errors = typeCheckResult.left.get

    val expected = List(
      CompilerMessage(CompilerError, TypeCheckingError, Some(LineNumber(13)), "Incompatible types in assignment statement for variable \"num_aux\".\nExpected:  int\nFound:     boolean"),
      CompilerMessage(CompilerError, TypeCheckingError, Some(LineNumber(15)), "Incompatible types in assignment statement for variable \"num_aux2\".\nExpected:  boolean\nFound:     int"),
      CompilerMessage(CompilerError, TypeCheckingError, Some(LineColumn(17, 18)), "Found instance of + with parameters of type \"int\" and \"boolean\". No version of the operation was found for this type combination.\n\nValid type combinations for this operator are:\n\n\"int\" and \"int\"")
    )

    errors shouldBe expected
  }

  it should "fail when a type check error exists in the main method" in {
    val input = Main.readFile("examples/HelloTrue.minijava")

    val ast = Main.parseString(input)

    ast.isRight shouldBe true

    val typeCheckResult = TypeChecking.typeCheck(ast.right.get)

    typeCheckResult.isLeft shouldBe true

    val errors = typeCheckResult.left.get

    val expected = List(
      CompilerMessage(CompilerError, TypeCheckingError, Some(LineNumber(3)), "Incompatible types in print statement.\nExpected:  int\nFound:     boolean")
    )

    errors shouldBe expected
  }

  it should "fail when an operator is used with a wrong parameter type" in {
    val input = Main.readFile("examples/PlusBoolean.minijava")

    val ast = Main.parseString(input)

    ast.isRight shouldBe true

    val typeCheckResult = TypeChecking.typeCheck(ast.right.get)

    typeCheckResult.isLeft shouldBe true

    val errors = typeCheckResult.left.get

    val expected = List(
      CompilerMessage(CompilerError, TypeCheckingError, Some(LineColumn(3, 27)), "Found instance of + with parameters of type \"int\" and \"boolean\". No version of the operation was found for this type combination.\n\nValid type combinations for this operator are:\n\n\"int\" and \"int\"")
    )

    errors shouldBe expected
  }

  it should "pass the Factorial example" in {
    val input = Main.readFile("examples/Factorial.minijava")

    val ast = Main.parseString(input)

    ast.isRight shouldBe true

    val typeCheckResult = TypeChecking.typeCheck(ast.right.get)

    typeCheckResult.isRight shouldBe true

    val typeTable = typeCheckResult.right.get

    typeTable.get("Factorial").isDefined shouldBe true
    typeTable.get("Fac").isDefined shouldBe true
  }

  it should "pass the BinarySearch example" in {
    val input = Main.readFile("examples/BinarySearch.minijava")

    val ast = Main.parseString(input)

    ast.isRight shouldBe true

    val typeCheckResult = TypeChecking.typeCheck(ast.right.get)

    typeCheckResult.isRight shouldBe true

    val typeTable = typeCheckResult.right.get

    typeTable.get("BinarySearch").isDefined shouldBe true
    typeTable.get("BS").isDefined shouldBe true
  }

  it should "pass the BubbleSort example" in {
    val input = Main.readFile("examples/BubbleSort.minijava")

    val ast = Main.parseString(input)

    ast.isRight shouldBe true

    val typeCheckResult = TypeChecking.typeCheck(ast.right.get)

    typeCheckResult.isRight shouldBe true

    val typeTable = typeCheckResult.right.get

    typeTable.get("BubbleSort").isDefined shouldBe true
    typeTable.get("BBS").isDefined shouldBe true
  }

  it should "pass the TreeVisitor example" in {
    val input = Main.readFile("examples/TreeVisitor.minijava")

    val ast = Main.parseString(input)

    ast.isRight shouldBe true

    val typeCheckResult = TypeChecking.typeCheck(ast.right.get)

    typeCheckResult.isRight shouldBe true

    val typeTable = typeCheckResult.right.get

    typeTable.get("TreeVisitor").isDefined shouldBe true
    typeTable.get("TV").isDefined shouldBe true
    typeTable.get("Tree").isDefined shouldBe true
    typeTable.get("Visitor").isDefined shouldBe true
    typeTable.get("MyVisitor").isDefined shouldBe true
  }

  it should "pass the QuickSort example" in {
    val input = Main.readFile("examples/QuickSort.minijava")

    val ast = Main.parseString(input)

    ast.isRight shouldBe true

    val typeCheckResult = TypeChecking.typeCheck(ast.right.get)

    typeCheckResult.isRight shouldBe true

    val typeTable = typeCheckResult.right.get

    typeTable.get("QuickSort").isDefined shouldBe true
    typeTable.get("QS").isDefined shouldBe true
  }

  it should "pass the LinearSearch example" in {
    val input = Main.readFile("examples/LinearSearch.minijava")

    val ast = Main.parseString(input)

    ast.isRight shouldBe true

    val typeCheckResult = TypeChecking.typeCheck(ast.right.get)

    typeCheckResult.isRight shouldBe true

    val typeTable = typeCheckResult.right.get

    typeTable.get("LinearSearch").isDefined shouldBe true
    typeTable.get("LS").isDefined shouldBe true
  }

  it should "pass the LinkedList example" in {
    val input = Main.readFile("examples/LinkedList.minijava")

    val ast = Main.parseString(input)

    ast.isRight shouldBe true

    val typeCheckResult = TypeChecking.typeCheck(ast.right.get)

    typeCheckResult.isRight shouldBe true

    val typeTable = typeCheckResult.right.get

    typeTable.get("LinkedList").isDefined shouldBe true
    typeTable.get("Element").isDefined shouldBe true
    typeTable.get("List").isDefined shouldBe true
    typeTable.get("LL").isDefined shouldBe true
  }

  it should "pass the BinaryTree example" in {
    val input = Main.readFile("examples/BinaryTree.minijava")

    val ast = Main.parseString(input)

    ast.isRight shouldBe true

    val typeCheckResult = TypeChecking.typeCheck(ast.right.get)

    typeCheckResult.isRight shouldBe true

    val typeTable = typeCheckResult.right.get

    typeTable.get("BinaryTree").isDefined shouldBe true
    typeTable.get("BT").isDefined shouldBe true
    typeTable.get("Tree").isDefined shouldBe true
  }

  it should "pass the HelloOne example" in {
    val input = Main.readFile("examples/HelloOne.minijava")

    val ast = Main.parseString(input)

    ast.isRight shouldBe true

    val typeCheckResult = TypeChecking.typeCheck(ast.right.get)

    typeCheckResult.isRight shouldBe true

    val typeTable = typeCheckResult.right.get

    typeTable.get("HelloOne").isDefined shouldBe true
  }
}
