package minijava

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}

import org.antlr.v4.runtime._
import minijava.grammar._
import minijava.messages.{CompilerError, CompilerMessage, CompilerWarning}
import minijava.parser.{MiniJavaVisitorImpl, ParseErrorListener}

object Main {
  def main(args: Array[String]): Unit = {
    if (args.length < 1) {
      println("%s[error]%s No source file provided. I don't know which file to compile.\n".format(Console.RED, Console.RESET))
      println("Be sure to provide the source file to compile as the first command line argument.")
      System.exit(1)
    }

    val filepath = args.head

    val input = readFile(filepath)

    val ast = (parseString(input) match {
      case Right(a) => a
      case Left(errors) =>
        printParserErrors(errors)
        System.exit(1)
    }).asInstanceOf[Goal]

    checkASTAndPrettied(ast, input)
  }

  def parseString(input: String): Either[List[CompilerMessage], Goal] = {
    val charStream = CharStreams.fromString(input)

    parse(charStream)
  }

  def parse(charStream: CharStream): Either[List[CompilerMessage], Goal] = {
    val lexer = new MiniJavaLexer(charStream)

    val listener = new ParseErrorListener()
    val tokens = new CommonTokenStream(lexer)
    val parser = new MiniJavaParser(tokens)

    parser.removeErrorListeners()
    parser.addErrorListener(listener)

    val visitor = new MiniJavaVisitorImpl()

    val goal = visitor.visit(parser.goal).asInstanceOf[Option[Goal]]

    if (listener.getErrors().nonEmpty) {
      Left(listener.getErrors())
    } else {
      Right(goal.get)
    }
  }

  def readFile(filepath: String): String = {
    new String(Files.readAllBytes(Paths.get(filepath)), StandardCharsets.UTF_8)
  }

  def printParserErrors(errors: List[CompilerMessage]): Unit = {
    errors.foreach(e => println(e.toDisplayString()))

    val numErrors = errors.count(_.kind == CompilerError)
    val numWarnings = errors.count(_.kind == CompilerWarning)

    val errorsMsg = if (numErrors > 0) "\n%s%d error(s)%s".format(Console.RED, numErrors, Console.RESET) else ""
    val warningsMsg = if (numWarnings > 0) "\n%s%d warnings(s)%s".format(Console.YELLOW, numWarnings, Console.RESET) else ""

    println(errorsMsg + warningsMsg)
  }

  def checkASTAndPrettied(ast: Goal, input: String): Unit = {
    /*println("----------------")
    println("|   Original   |")
    println("----------------")
    println(input)
    println("----------------")
    println("|     AST      |")
    println("----------------")
    println(ast)
    println("----------------")
    println("|    Pretty    |")
    println("----------------")*/

    val prettied = AST.prettyPrint(ast)
    /*println(prettied)

    println("----------------")
    println("|   Re-Pretty  |")
    println("----------------")*/

    val prettiedAst = (parseString(prettied) match {
      case Right(a) => a
      case Left(errors) =>
        printParserErrors(errors)
        System.exit(1)
    }).asInstanceOf[Goal]
    val doublePrettied = AST.prettyPrint(prettiedAst)
    /*println(doublePrettied)

    println("----------------")*/

    val compareASTs = ast.equals(prettiedAst)
    println("AST from prettied same as non-prettied:                     \t%s".format(compareASTs))

    val comparePrettied = prettied.equals(doublePrettied)
    println("Prettied same as double prettied:                           \t%s".format(comparePrettied))

    val originalNoSpacesOrComments = input
      .split("\n")
      .map(l => l.split("//").head) // Remove comments
      .mkString("")
      .replaceAll("\\s", "")    // Remove whitespace
    val prettiedNoSpaces = prettied.replaceAll("\\s", "")

    val compareNoSpaces = prettiedNoSpaces.equals(originalNoSpacesOrComments)
    println("Prettied same as original, ignoring whitespace + comments:  \t%s".format(compareNoSpaces))

    if (!compareNoSpaces) {
      println("diff:")
      println("\"%s\"".format(originalNoSpacesOrComments.diff(prettiedNoSpaces)))
      println("diff (op):")
      println("\"%s\"".format(prettiedNoSpaces.diff(originalNoSpacesOrComments)))
    }

    assert(compareASTs)
    assert(comparePrettied)
    assert(compareNoSpaces)
  }
}
