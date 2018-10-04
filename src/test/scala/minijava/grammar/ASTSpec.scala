package minijava.grammar

import minijava.Main
import org.scalatest._

class ASTSpec extends FlatSpec with Matchers {
  "prettyPrint" should "should return a non-empty string for the Factorial example" in {
    val input = Main.readFile("examples/Factorial.minijava")

    val output = Main.parseString(input)

    output.isRight shouldBe true

    val ast = output.right.get

    val output2 = AST.prettyPrint(ast)

    (output2.length > 0) shouldBe true
  }

  it should "should return a non-empty string for the TreeVisitor example" in {
    val input = Main.readFile("examples/TreeVisitor.minijava")

    val output = Main.parseString(input)

    output.isRight shouldBe true

    val ast = output.right.get

    val output2 = AST.prettyPrint(ast)

    (output2.length > 0) shouldBe true
  }

  it should "should return a non-empty string for the BinarySearch example" in {
    val input = Main.readFile("examples/BinarySearch.minijava")

    val output = Main.parseString(input)

    output.isRight shouldBe true

    val ast = output.right.get

    val output2 = AST.prettyPrint(ast)

    (output2.length > 0) shouldBe true
  }
}
