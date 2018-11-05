package minijava.codegeneration

import java.io.FileOutputStream

import minijava.Main
import minijava.typechecking.{MainClassType, TypeChecking}
import org.scalatest._
import sys.process._


class CodeGenerationSpec extends FlatSpec with Matchers {
  "CodeGenerationVisitor" should "work on the HelloOne example" in {
    val input = Main.readFile("examples/HelloOne.minijava")

    val ast = Main.parseString(input)

    ast.isRight shouldBe true

    val typeCheckResult = TypeChecking.typeCheck(ast.right.get)

    typeCheckResult.isRight shouldBe true

    val typeTable = typeCheckResult.right.get

    val vistor = new CodeGenerationVisitor()

    val mainClassType = typeTable.get("HelloOne").get.asInstanceOf[MainClassType]

    vistor.visitMainClassType("HelloOne.minijava", mainClassType)

    try {
      val fos = new FileOutputStream("HelloOne.class")
      try
        fos.write(vistor.getClassWriter().toByteArray)
      //fos.close(); There is no more need for this line since you had created the instance of "fos" inside the try. And this will automatically close the OutputStream
      finally if (fos != null) fos.close()
    }

    testOutput("HelloOne", "1\n")
  }

  it should "work on the HelloTwo example" in {
    val input = Main.readFile("examples/HelloTwo.minijava")

    val ast = Main.parseString(input)

    ast.isRight shouldBe true

    val typeCheckResult = TypeChecking.typeCheck(ast.right.get)

    typeCheckResult.isRight shouldBe true

    val typeTable = typeCheckResult.right.get

    val vistor = new CodeGenerationVisitor()

    val mainClassType = typeTable.get("HelloTwo").get.asInstanceOf[MainClassType]

    vistor.visitMainClassType("HelloTwo.minijava", mainClassType)

    try {
      val fos = new FileOutputStream("HelloTwo.class")
      try
        fos.write(vistor.getClassWriter().toByteArray)
      //fos.close(); There is no more need for this line since you had created the instance of "fos" inside the try. And this will automatically close the OutputStream
      finally if (fos != null) fos.close()
    }

    testOutput("HelloTwo", "2\n")
  }

  def testOutput(mainClassName: String, expectedOutput: String): Assertion = {
    val command = f"java $mainClassName"

    val actualOutput = command !!

    actualOutput shouldBe expectedOutput
  }
}
