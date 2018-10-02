package minijava

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}

import org.antlr.v4.runtime._
import minijava.grammar._
import minijava.parser.MiniJavaVisitorImpl

object Main {
  def main(args: Array[String]): Unit = {
    assert(args.length > 0)

    val filepath = args.head

    val input = readFile(filepath)

    val ast = parseString(input)

    println("----------------")
    println("|   Original   |")
    println("----------------")
    println(input)
    println("----------------")
    println("|     AST      |")
    println("----------------")
    println(ast)
    println("----------------")
    println("|    Pretty    |")
    println("----------------")

    val prettied = AST.prettyPrint(ast)
    println(prettied)

    println("----------------")
    println("|   Re-Pretty  |")
    println("----------------")

    val prettiedAst = parseString(prettied)
    val doublePrettied = AST.prettyPrint(prettiedAst)
    println(doublePrettied)

    println("----------------")

    val compareASTs = ast.equals(prettiedAst)
    println("AST from prettied same as non-prettied:                     \t%s".format(compareASTs))

    val comparePrettied = prettied.equals(doublePrettied)
    println("Prettied same as double prettied:                           \t%s".format(comparePrettied))

    val originalNoSpacesOrComments = input
      .split("\n")
      .filter(!_.contains("//"))
      .mkString("\n")
      .replaceAll("\\s", "")
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

  def parseString(input: String): Goal = {
    val charStream = CharStreams.fromString(input)

    parse(charStream)
  }

  def parse(charStream: CharStream): Goal = {
    val lexer = new MiniJavaLexer(charStream)
    val tokens = new CommonTokenStream(lexer)
    val parser = new MiniJavaParser(tokens)

    val visitor = new MiniJavaVisitorImpl()

    visitor.visit(parser.goal).asInstanceOf[Goal]
  }

  def readFile(filepath: String): String = {
    new String(Files.readAllBytes(Paths.get(filepath)), StandardCharsets.UTF_8)
  }
}
