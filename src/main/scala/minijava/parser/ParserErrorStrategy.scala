package minijava.parser

import org.antlr.v4.runtime.misc.ParseCancellationException
import org.antlr.v4.runtime._

import scala.collection.mutable.ArrayBuffer

class ParserErrorStrategy extends DefaultErrorStrategy {
  private val accumulatedErrors = ArrayBuffer[String]()

  /*override def recover(recognizer: Parser, e: RecognitionException): Unit = {
    //recognizer.getContext

    val error = new StringBuilder()

    error.append("a: %s".format(e.getCtx.getSourceInterval.a))
    error.append("b: %s".format(e.getCtx.getSourceInterval.b))

    accumulatedErrors.append(error.toString())

    //throw new RecognitionException(recognizer, recognizer.getInputStream, recognizer.getContext)

    //throw new InputMismatchException(recognizer)

    // TODO: remove
    import org.antlr.v4.runtime.ParserRuleContext
    import org.antlr.v4.runtime.misc.ParseCancellationException
    var context = recognizer.getContext
    while ( {
      context != null
    }) {
      context.exception = e

      context = context.getParent
    }

    throw new ParseCancellationException(e)
  }

  override def recoverInline(recognizer: Parser): Token = {
    //throw new RecognitionException(recognizer, recognizer.getInputStream, recognizer.getContext)

    //throw new InputMismatchException(recognizer)

    // TODO: remove
    import org.antlr.v4.runtime.ParserRuleContext
    import org.antlr.v4.runtime.misc.ParseCancellationException
    val e = new InputMismatchException(recognizer)
    var context = recognizer.getContext
    while ( {
      context != null
    }) {
      context.exception = e

      context = context.getParent
    }

    throw new ParseCancellationException(e)
  }

  def getErrors(): List[String] = {
    accumulatedErrors.toList
  }*/

  override def recover(recognizer: Parser, e: RecognitionException): Unit = {
    println("RECOVER")

    throw e
  }

  override def reportInputMismatch(recognizer: Parser, e: InputMismatchException): Unit = {
    val error = new StringBuilder()

    println("YAAAAAAAAAY")

    error.append("mismatched input")

    val ex = new RecognitionException(error.toString(), recognizer, recognizer.getInputStream(), recognizer.getContext())

    throw ex
  }
}
