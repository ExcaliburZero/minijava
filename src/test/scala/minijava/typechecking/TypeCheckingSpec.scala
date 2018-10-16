package minijava.typechecking

import minijava.Main
import minijava.messages.{CompilerError, CompilerMessage, LineNumber, TypeCheckingError}
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

  it should "pass the BinarySearch example" in {
    val input = Main.readFile("examples/BinarySearch.minijava")

    val ast = Main.parseString(input)

    ast.isRight shouldBe true

    val typeCheckResult = TypeChecking.typeCheck(ast.right.get)

    typeCheckResult.isRight shouldBe true

    val typeTable = typeCheckResult.right.get

    typeTable.get("BS").isDefined shouldBe true
  }
}
