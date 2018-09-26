package minijava

import org.antlr.v4.runtime._

import minijava.grammar._

object Main {
  def main(args: Array[String]): Unit = {
    parse("""
class Factorial{
    public static void main(String[] a){
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
      """)
  }

  def parse(input: String): Unit = {
    val charStream = new ANTLRInputStream(input)
    val lexer = new MiniJavaLexer(charStream)
    val tokens = new CommonTokenStream(lexer)
    val parser = new MiniJavaParser(tokens)

	val context = parser.goal

	println(context)
  }
}
