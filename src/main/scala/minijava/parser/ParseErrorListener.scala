package minijava.parser

import minijava.messages.{CompilerError, CompilerMessage, LineColumn, ParsingError}
import org.antlr.v4.runtime._

import scala.collection.mutable.ArrayBuffer

/**
  * An error listener that records all of the encountered syntax errors into
  * an ArrayBuffer, so that they can be accessed after the parsing is
  * performed.
  *
  * Note that one instance should be used per parse, as it accumulates the
  * errors and does not clear them out after completing the parse.
  */
class ParseErrorListener() extends BaseErrorListener {
  private val accumulatedErrors = ArrayBuffer[CompilerMessage]()

  override def syntaxError(recognizer: Recognizer[_, _], offendingSymbol: scala.Any, line: Int, charPositionInLine: Int, msg: String, e: RecognitionException): Unit = {
    val location = LineColumn(line, charPositionInLine)
    accumulatedErrors.append(CompilerMessage(CompilerError, ParsingError, Some(location), msg))
  }

  def getErrors(): List[CompilerMessage] = {
    accumulatedErrors.toList
  }
}
