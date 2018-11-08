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

  it should "should properly display a TypeChecking error" in {
    val message = CompilerMessage(CompilerError, TypeCheckingError, Some(LineColumn(2, 4)), "Incompatible types in negated expression.\nExpected:  boolean\nFound:     int")

    val output = message.toDisplayString()
    val expected = "%s[type checking] %s%s[error] %sln:2 col:4 - Incompatible types in negated expression.\nExpected:  boolean\nFound:     int".format(
      Console.BLUE, Console.RESET, Console.RED, Console.RESET
    )

    output shouldBe expected
  }

  it should "should properly display an error with just a line number" in {
    val message = CompilerMessage(CompilerError, TypeCheckingError, Some(LineNumber(5)), "Incompatible types in negated expression.\nExpected:  boolean\nFound:     int")

    val output = message.toDisplayString()
    val expected = "%s[type checking] %s%s[error] %sln:5 - Incompatible types in negated expression.\nExpected:  boolean\nFound:     int".format(
      Console.BLUE, Console.RESET, Console.RED, Console.RESET
    )

    output shouldBe expected
  }

  it should "should properly display an error with no location information" in {
    val message = CompilerMessage(CompilerError, TypeCheckingError, None, "Incompatible types in negated expression.\nExpected:  boolean\nFound:     int")

    val output = message.toDisplayString()
    val expected = "%s[type checking] %s%s[error] %s- Incompatible types in negated expression.\nExpected:  boolean\nFound:     int".format(
      Console.BLUE, Console.RESET, Console.RED, Console.RESET
    )

    output shouldBe expected
  }
}
