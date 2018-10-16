package minijava.messages

import org.scalatest._

class CompilerMessagesSpec extends FlatSpec with Matchers {
  "toDisplayString" should "should properly display a parsing/lexing error" in {
    val message = CompilerMessage(CompilerError, ParsingError, Some(LineColumn(2, 4)), "missing '{' at 'public'")

    val output = message.toDisplayString()
    val expected = "%s[parsing/lexing] %s%s[error] %sln:2 col:4 - missing '{' at 'public'".format(
      Console.YELLOW, Console.RESET, Console.RED, Console.RESET
    )

    output shouldBe expected
  }

  it should "should properly display a parsing/lexing warning" in {
    val message = CompilerMessage(CompilerWarning, ParsingError, Some(LineColumn(2, 4)), "missing '{' at 'public'")

    val output = message.toDisplayString()
    val expected = "%s[parsing/lexing] %s%s[warning] %sln:2 col:4 - missing '{' at 'public'".format(
      Console.YELLOW, Console.RESET, Console.YELLOW, Console.RESET
    )

    output shouldBe expected
  }
}
