package minijava

import java.io.FileOutputStream
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, NoSuchFileException, Paths}

import minijava.codegeneration.CodeGenerationVisitor
import minijava.grammar._
import minijava.messages.{CompilerError, CompilerMessage, CompilerWarning}
import minijava.optimization.Optimization
import minijava.parser.{MiniJavaVisitorImpl, ParseErrorListener}
import minijava.typechecking.{ClassType, MainClassType, TypeChecking, TypeTable}
import org.antlr.v4.runtime._

object Main {
  def main(args: Array[String]): Unit = {
    //NullCheckOptimization.main(Array())

    //???

    if (args.length < 1) {
      println("%s[error]%s No source file provided. I don't know which file to compile.\n".format(Console.RED, Console.RESET))
      println("Be sure to provide the source file to compile as the first command line argument.")
      System.exit(1)
    }

    val filepath = args.head
    compile(filepath)
  }

  def compile(filepath: String): Unit = {
    val input = (try {
      readFile(filepath)
    } catch {
      case e: NoSuchFileException =>
        println("%s[error]%s Provided file \"%s\" does not exist.\n".format(Console.RED, Console.RESET, filepath))
        System.exit(1)
    }).asInstanceOf[String]

    val ast = (parseString(input) match {
      case Right(a) => a
      case Left(errors) =>
        printErrors(errors)

        if (errors.exists(_.kind == CompilerError)) {
          System.exit(1)
        }
    }).asInstanceOf[Goal]

    val typeTable = (TypeChecking.typeCheck(ast) match {
      case (Some(t), None) => t
      case (t, Some(errors)) =>
        printErrors(errors)

        if (errors.exists(_.kind == CompilerError)) {
          System.exit(1)
        } else {
          t.get
        }
    }).asInstanceOf[TypeTable]

    val mainClassName = ast.mainClass.name.name

    val classFiles = generateCode(typeTable, mainClassName, filepath)

    Optimization.copyClassFilesForOptimization(classFiles)
    classFiles.foreach(Optimization.optimizeFile)

    printLogo()
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

    if (listener.getErrors().nonEmpty || visitor.getASTErrors().nonEmpty) {
      Left(
        listener.getErrors() ++ visitor.getASTErrors()
      )
    } else {
      Right(goal.get)
    }
  }

  def readFile(filepath: String): String = {
    new String(Files.readAllBytes(Paths.get(filepath)), StandardCharsets.UTF_8)
  }

  def generateCode(typeTable: TypeTable, mainClassName: String, filepath: String): List[String] = {
    val visitor = new CodeGenerationVisitor()

    val mainClassType = typeTable.get(mainClassName).get.asInstanceOf[MainClassType]
    visitor.visitMainClassType(filepath, mainClassType)

    typeTable.types()
      .filter(_.isInstanceOf[ClassType])
      .map(_.asInstanceOf[ClassType])
      .foreach(visitor.visitClassType(filepath, _))

    writeClassFiles(visitor)

    visitor.getClassWriters().map(_._1 + ".class")
  }

  private def writeClassFiles(visitor: CodeGenerationVisitor): Unit = {
    for ((classFileName, classWriter) <- visitor.getClassWriters()) {
      val fos = new FileOutputStream(f"$classFileName.class")
      try {
        fos.write(classWriter.toByteArray)
      }
      finally if (fos != null) fos.close()
    }
  }

  /**
    * Prints out the given list of CompilerMessages.
    *
    * @param errors The CompilerMessages to print.
    */
  def printErrors(errors: List[CompilerMessage]): Unit = {
    errors.foreach(e => println(e.toDisplayString()))

    val numErrors = errors.count(_.kind == CompilerError)
    val numWarnings = errors.count(_.kind == CompilerWarning)

    val errorsMsg = if (numErrors > 0) "\n%s%d error(s)%s".format(Console.RED, numErrors, Console.RESET) else ""
    val warningsMsg = if (numWarnings > 0) "\n%s%d warnings(s)%s".format(Console.YELLOW, numWarnings, Console.RESET) else ""

    println(errorsMsg + warningsMsg)
  }

  private def printLogo(): Unit = {
    println(
      """

           ;k'        ,,
          ok        '0:
         ,0        ,0.
         ol        O,
         ;0        k:
          kl       'K.
           0:       :0
           .X.       Oc
            :O       .K,
             N        'N
            .X        .M.
           .K,        xd
          ck.       ;d'

                    ..          .x  .:olll;
  ,k             ,dd;;dx.       .K.xo.    'K'
  lo oxclk,   .cx:      lxl',dd..WO.       K,
  ll      :oloc.          .,'   ok        ok
  ll                            K;    ,ldx;
  ;k                           .N  ,dd,.
  .X          /\ /\            dxlk,
   dd        /  V  \          .WK,
    0:      /       \ ini     kk
    .K'                      xx
     .K'                    lk
       xd.                .dx
        .oxc            :xo.
           .;lllllclllol'

      """
    )
  }
}
