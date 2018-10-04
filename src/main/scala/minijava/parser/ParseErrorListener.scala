package minijava.parser

import java.util

import minijava.messages.{CompilerError, CompilerMessage, ParsingError}
import org.antlr.v4.runtime._

import scala.collection.mutable.ArrayBuffer

class ParseErrorListener() extends BaseErrorListener {
  private val accumulatedErrors = ArrayBuffer[CompilerMessage]()

  override def syntaxError(recognizer: Recognizer[_, _], offendingSymbol: scala.Any, line: Int, charPositionInLine: Int, msg: String, e: RecognitionException): Unit = {
    val error = new StringBuilder()

    val eActual = if (e != null) {
      e
    } else {
      new RecognitionException(msg, recognizer, recognizer.getInputStream, recognizer.asInstanceOf[Parser].getContext)
    }

    error.append("%d:%d: %s".format(line, charPositionInLine, msg))

    accumulatedErrors.append(CompilerMessage(CompilerError, ParsingError, line, charPositionInLine, msg))
  }

  private def getTokenString(tokenId: Int, recognizer: Recognizer[_, _]): Option[String] = {
    recognizer.getTokenTypeMap.entrySet().toArray
      .map(t => t.asInstanceOf[util.Map.Entry[String, Integer]])
      .filter(e => e.getValue == tokenId)
      .map(e => e.getKey)
      .headOption
  }

  def getErrors(): List[CompilerMessage] = {
    accumulatedErrors.toList
  }
}
