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
  /*"typeCheck" should "pass the BinarySearch example" in {
    val input = Main.readFile("examples/BinarySearch.minijava")

    val ast = Main.parseString(input)

    ast.isRight shouldBe true

    val typeCheckResult = TypeChecking.typeCheck(ast.right.get)

    typeCheckResult.isRight shouldBe true
  }*/
}
