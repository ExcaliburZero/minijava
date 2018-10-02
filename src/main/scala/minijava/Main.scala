package minijava

import org.antlr.v4.runtime._
import minijava.grammar._
import minijava.parser.MiniJavaVisitorImpl

object Main {
  def main(args: Array[String]): Unit = {
    val input = """
class Factorial{
    public static io void main(String[] a){
	System.out.println(new Fac().ComputeFac(10));
    }
}

class Fac {

    public int ComputeFac(int num){
	int num_aux ;
	if (num < 1)
	    num_aux = 1 ;
	else
	    num_aux = num * (this.ComputeFac(num-1)) ;
	return num_aux ;
    }

}
      """

    val ast = parse(input)

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

    val prettiedAst = parse(prettied)
    val doublePrettied = AST.prettyPrint(prettiedAst)
    println(doublePrettied)

    println("----------------")

    val compareASTs = ast.equals(prettiedAst)
    println("AST from prettied same as non-prettied:\t%s".format(compareASTs))

    val comparePrettied = prettied.equals(doublePrettied)
    println("Prettied same as double prettied:      \t%s".format(comparePrettied))

    assert(compareASTs)
    assert(comparePrettied)
  }

  def parse(input: String): Goal = {
    val charStream = new ANTLRInputStream(input)
    val lexer = new MiniJavaLexer(charStream)
    val tokens = new CommonTokenStream(lexer)
    val parser = new MiniJavaParser(tokens)

    val visitor = new MiniJavaVisitorImpl()

    visitor.visit(parser.goal).asInstanceOf[Goal]
  }
}
