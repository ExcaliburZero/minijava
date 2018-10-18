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

  it should "pass the HelloOne example" in {
    val input = Main.readFile("examples/HelloOne.minijava")

    val ast = Main.parseString(input)

    ast.isRight shouldBe true

    val typeCheckResult = TypeChecking.typeCheck(ast.right.get)

    typeCheckResult.isRight shouldBe true

    val typeTable = typeCheckResult.right.get

    typeTable.get("HelloOne").isDefined shouldBe true
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

  /*it should "pass the BinarySearch example" in {
    val input = Main.readFile("examples/BinarySearch.minijava")

    val ast = Main.parseString(input)

    ast.isRight shouldBe true

    val typeCheckResult = TypeChecking.typeCheck(ast.right.get)

    typeCheckResult.isRight shouldBe true

    val typeTable = typeCheckResult.right.get

    typeTable.get("BS").isDefined shouldBe true
  }*/
}
