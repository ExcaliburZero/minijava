package minijava

import minijava.messages.{CompilerError, CompilerMessage, LineColumn, ParsingError}
import org.scalatest._

class MainSpec extends FlatSpec with Matchers {
  "parseString" should "should successfully parse the Factorial example" in {
    val input = Main.readFile("examples/Factorial.minijava")

    val output = Main.parseString(input)

    output.isRight shouldBe true
  }

  it should "should successfully parse the BinarySearch example" in {
    val input = Main.readFile("examples/BinarySearch.minijava")

    val output = Main.parseString(input)

    output.isRight shouldBe true
  }

  it should "should successfully parse the BubbleSort example" in {
    val input = Main.readFile("examples/BubbleSort.minijava")

    val output = Main.parseString(input)

    output.isRight shouldBe true
  }

  it should "should successfully parse the TreeVisitor example" in {
    val input = Main.readFile("examples/TreeVisitor.minijava")

    val output = Main.parseString(input)

    output.isRight shouldBe true
  }

  it should "should successfully parse the QuickSort example" in {
    val input = Main.readFile("examples/QuickSort.minijava")

    val output = Main.parseString(input)

    output.isRight shouldBe true
  }

  it should "should successfully parse the LinearSearch example" in {
    val input = Main.readFile("examples/LinearSearch.minijava")

    val output = Main.parseString(input)

    output.isRight shouldBe true
  }

  it should "should successfully parse the LinkedList example" in {
    val input = Main.readFile("examples/LinkedList.minijava")

    val output = Main.parseString(input)

    output.isRight shouldBe true
  }

  it should "should successfully parse the BinaryTree example" in {
    val input = Main.readFile("examples/BinaryTree.minijava")

    val output = Main.parseString(input)

    output.isRight shouldBe true
  }

  it should "should fail to parse the MissingOpeningCurly example" in {
    val input = Main.readFile("examples/MissingOpeningCurly.minijava")

    val output = Main.parseString(input)

    output.isLeft shouldBe true
    output.left.get shouldBe List(CompilerMessage(CompilerError, ParsingError, Some(LineColumn(2, 4)), "missing '{' at 'public'"))
  }

  it should "should fail to parse the MissingClosingCurly example" in {
    val input = Main.readFile("examples/MissingClosingCurly.minijava")

    val output = Main.parseString(input)

    output.isLeft shouldBe true
    output.left.get shouldBe List(CompilerMessage(CompilerError, ParsingError, Some(LineColumn(10, 0)), "missing '}' at 'class'"))
  }

  it should "should fail to parse the MissingLots example" in {
    val input = Main.readFile("examples/MissingLots.minijava")

    val output = Main.parseString(input)

    output.isLeft shouldBe true
  }
}
