package minijava.parser

import minijava.messages.{CompilerError, CompilerMessage, ParsingError}
import org.antlr.v4.runtime._

import scala.collection.mutable.ArrayBuffer

class ParseErrorListener() extends BaseErrorListener {
  private val accumulatedErrors = ArrayBuffer[CompilerMessage]()

  override def syntaxError(recognizer: Recognizer[_, _], offendingSymbol: scala.Any, line: Int, charPositionInLine: Int, msg: String, e: RecognitionException): Unit = {
    accumulatedErrors.append(CompilerMessage(CompilerError, ParsingError, line, charPositionInLine, msg))
  }

  def getErrors(): List[CompilerMessage] = {
    accumulatedErrors.toList
  }
}
