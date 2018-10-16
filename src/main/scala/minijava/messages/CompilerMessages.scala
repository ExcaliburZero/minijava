package minijava.messages

sealed trait MessageSource
case object ParsingError extends MessageSource
case object TypeCheckingError extends MessageSource

sealed trait MessageKind
case object CompilerError extends MessageKind
case object CompilerWarning extends MessageKind

sealed trait Location
case class LineColumn(line: Int, column: Int) extends Location
case class LineNumber(line: Int) extends Location

case class CompilerMessage(kind: MessageKind, source: MessageSource, location: Option[Location], message: String) {
  /**
    * Returns a pretty formatted string version of the CompilerMessage.
    *
    * @return A string representation of the CompilerMessage.
    */
  def toDisplayString(): String = {
    val display = new StringBuilder()

    val sourceOutput = source match {
      case ParsingError => "%s[parsing/lexing] %s".format(Console.YELLOW, Console.RESET)
      case TypeCheckingError => "%s[type checking] %s".format(Console.BLUE, Console.RESET)
    }
    display.append(sourceOutput)

    val kindOutput = kind match {
      case CompilerError => "%s[error] %s".format(Console.RED, Console.RESET)
      case CompilerWarning => "%s[warning] %s".format(Console.YELLOW, Console.RESET)
    }
    display.append(kindOutput)

    location match {
      case Some(LineColumn(line, column)) =>
        display.append("ln:%d col:%d ".format(line, column))
      case Some(LineNumber(line)) =>
        display.append("ln:%d ".format(line))
      case None => ()
    }

    display.append("- ")

    display.append(message)

    display.toString()
  }
}
