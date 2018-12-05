package minijava.optimization

import java.io.{FileOutputStream, OutputStreamWriter, PrintWriter}

import soot.options.Options
import soot.shimple.Shimple
import soot.toolkits.graph.ExceptionalUnitGraph
import soot.util.JasminOutputStream
import soot.{Scene, SootClass, SourceLocator}

import scala.collection.JavaConverters._

object NullCheckOptimization {
  def main(args: Array[String]): Unit = {
    addClassDir("./testing")

    val a = loadClassFromClassPath("MyClass")
    jimpleToShimple(a)

    removeNullChecks(a)

    writeToClassFile(a)
  }

  def removeNullChecks(sootClass: SootClass): Unit = {
    val b = sootClass.getMethods.get(1).retrieveActiveBody()

    //val sb = Shimple.v().newBody(b)

    println(b)

    val graph = new ExceptionalUnitGraph(b)

    val analysis = new NullCheckAnalysis(graph)

    val unitsToRemove = analysis.getUnitsToRemove()

    println(unitsToRemove)

    val units = b.getUnits
    //units.clear()

    for (u <- unitsToRemove) {
      units.remove(u)
    }

    println(b)
  }

  def addClassDir(classDir: String): Unit = {
    Options.v().set_process_dir(List(classDir).asJava)
  }

  def loadClassFromClassPath(className: String): SootClass = {
    Scene.v().loadClassAndSupport(className)
    Scene.v().loadNecessaryClasses()

    Scene.v().getSootClass(className)
  }

  def writeToClassFile(sootClass: SootClass): Unit = {
    val fileName = SourceLocator.v().getFileNameFor(sootClass, Options.output_format_class)

    val streamOut = new JasminOutputStream(new FileOutputStream(fileName))
    val writerOut = new PrintWriter(new OutputStreamWriter(streamOut))

    val jasminClass = new soot.jimple.JasminClass(sootClass)
    jasminClass.print(writerOut)
    writerOut.flush()
    streamOut.close()

    println(s"Wrote to $fileName")
  }

  /**
    * Converts the method bodies of the given SootClass from Jimple to Shimple. This works by mutating the object in
    * place.
    *
    * @param jimpleClass The class to convert from Jimple to Shimple.
    */
  def jimpleToShimple(jimpleClass: SootClass): Unit = {
    for (method <- jimpleClass.getMethods.asScala) {
      val jimpleBody = method.retrieveActiveBody()

      val shimpleBody = Shimple.v().newBody(jimpleBody)
      shimpleBody.addAllTagsOf(jimpleBody)

      method.setActiveBody(shimpleBody)
    }
  }
}
