package minijava.optimization

import java.io.{File, FileOutputStream, OutputStreamWriter, PrintWriter}
import java.nio.file.{Files, StandardCopyOption}

import soot.options.Options
import soot.shimple.Shimple
import soot.toolkits.graph.ExceptionalUnitGraph
import soot.util.JasminOutputStream
import soot.{Scene, SootClass}

import scala.collection.JavaConverters._

object NullCheckOptimization {
  /**
    * The temporary directory to put class files in before attempting to optimize them. This allows them to be loaded
    * into the class path without accidentally loading in other files, which would lead to odd problems.
    */
  private val TMP_DIR = ".tmp"

  def copyClassFilesForOptimization(classFilePaths: List[String]): Unit = {
    softCreateDirectory(TMP_DIR)

    classFilePaths.foreach(copyFileToDir(_, TMP_DIR))
  }

  def optimizeFile(classFilePath: String): Unit = {
    println(s"Try optimize: $classFilePath")

    if (!isFileInDir(classFilePath, TMP_DIR)) {
      throw new Exception("Attempted to optimize class file \"" + classFilePath +
        "\", but this file has not yet been copied for optimization (by means of copyClassFilesForOptimization).")
    }

    addClassDir(TMP_DIR)

    val className = getFileNameWithoutExtension(classFilePath)

    val a = loadClassFromClassPath(className)
    jimpleToShimple(a)

    removeNullChecks(a)

    writeToClassFile(a, classFilePath)

    println(s"Wrote to: $classFilePath")
  }

  private def removeNullChecks(sootClass: SootClass): Unit = {
    // Convert the class to Shimple to take advantage of SSA
    jimpleToShimple(sootClass)

    for (m <- sootClass.getMethods.asScala) {
      val b = m.retrieveActiveBody()

      // Perform the control flow analysis to determine which null checks to remove
      val graph = new ExceptionalUnitGraph(b)
      val analysis = new NullCheckAnalysis(graph)
      val unitsToRemove = analysis.getUnitsToRemove()

      // Remove the unneeded null checks
      val units = b.getUnits
      for (u <- unitsToRemove) {
        units.remove(u)
      }
    }
  }

  private def softCreateDirectory(directoryPath: String): Unit = {
    val dir = new File(directoryPath)

    dir.mkdir()
  }

  private def copyFileToDir(filePath: String, directoryPath: String): Unit = {
    val file = new File(filePath)
    val dir = new File(directoryPath)

    val destinationFile = new File(dir, file.getName)

    Files.copy(file.toPath, destinationFile.toPath, StandardCopyOption.REPLACE_EXISTING)
  }

  private def addClassDir(classDir: String): Unit = {
    Options.v().set_process_dir(List(classDir).asJava)
  }

  private def loadClassFromClassPath(className: String): SootClass = {
    Scene.v().loadClassAndSupport(className)
    Scene.v().loadNecessaryClasses()

    Scene.v().getSootClass(className)
  }

  private def writeToClassFile(sootClass: SootClass, fileName: String): Unit = {
    //val fileName = SourceLocator.v().getFileNameFor(sootClass, Options.output_format_class)

    val streamOut = new JasminOutputStream(new FileOutputStream(fileName))
    val writerOut = new PrintWriter(new OutputStreamWriter(streamOut))

    val jasminClass = new soot.jimple.JasminClass(sootClass)
    jasminClass.print(writerOut)
    writerOut.flush()
    streamOut.close()
  }

  private def getFileNameWithoutExtension(filePath: String): String = {
    new File(filePath).getName.split("\\.").head
  }

  private def isFileInDir(filePath: String, dirPath: String): Boolean = {
    val fileName = new File(filePath).getName
    val dir = new File(dirPath)

    val desiredFile = new File(dir, fileName)

    desiredFile.exists()
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
