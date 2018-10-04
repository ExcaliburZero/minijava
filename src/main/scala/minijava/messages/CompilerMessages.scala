package minijava.messages

sealed trait MessageSource
case object ParsingError extends MessageSource

sealed trait MessageKind
case object CompilerError extends MessageKind

case class CompilerMessage(kind: MessageKind, source: MessageSource, line: Int, column: Int, message: String) {
  def toDisplayString(): String = {
    val display = new StringBuilder()

    val sourceOutput = source match {
      case ParsingError => "%s[parsing/lexing] %s".format(Console.YELLOW, Console.RESET)
    }
    display.append(sourceOutput)

    val kindOutput = kind match {
      case CompilerError => "%s[error] %s".format(Console.RED, Console.RESET)
    }
    display.append(kindOutput)

    display.append("ln:%d col:%d ".format(line, column))

    display.append("- ")

    display.append(message)

    display.toString()
  }
}
