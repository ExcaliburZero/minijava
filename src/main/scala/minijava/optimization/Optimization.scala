package minijava.optimization

import java.io.{File, FileOutputStream, OutputStreamWriter, PrintWriter}
import java.nio.file.{Files, StandardCopyOption}

import soot.options.Options
import soot.shimple.{Shimple, ShimpleBody}
import soot.toolkits.graph.ExceptionalUnitGraph
import soot.util.JasminOutputStream
import soot.{Scene, SootClass}

import scala.collection.JavaConverters._

object Optimization {
  /**
    * The temporary directory to put class files in before attempting to optimize them. This allows them to be loaded
    * into the class path without accidentally loading in other files, which would lead to odd problems.
    */
  private val TMP_DIR = ".tmp"

  /**
    * Copies the specified class files into a temporary directory that allows Soot to load them without causing class
    * path issues.
    *
    * This function must be run on each class file before it can be optimized.
    *
    * @param classFilePaths The class files to copy.
    */
  def copyClassFilesForOptimization(classFilePaths: List[String]): Unit = {
    softCreateDirectory(TMP_DIR)

    classFilePaths.foreach(copyFileToDir(_, TMP_DIR))
  }

  /**
    * Performs optimizations on the specified class file.
    *
    * @param classFilePath The file path of the class file to optimize.
    */
  def optimizeFile(classFilePath: String): Unit = {
    if (!isFileInDir(classFilePath, TMP_DIR)) {
      throw new Exception("Attempted to optimize class file \"" + classFilePath +
        "\", but this file has not yet been copied for optimization (by means of copyClassFilesForOptimization).")
    }

    addClassDir(TMP_DIR)

    val className = getFileNameWithoutExtension(classFilePath)
    val classObj = loadClassFromClassPath(className)

    jimpleToShimple(classObj)

    removeNullChecks(classObj)

    shimpleToJimple(classObj)

    writeToClassFile(classObj, classFilePath)
  }

  /**
    * Removes all of the unnecessary null checks from the given class by mutating its methods in place.
    *
    * @param sootClass The class to remove the unnecessary null checks of.
    */
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

  /**
    * Creates a new directory at the given location, does nothing if the directory already exists.
    *
    * @param directoryPath The path of the directory to create.
    */
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

  /**
    * Adds the given directory to the class path for Soot so that classes in the specified directory can be loaded in by
    * Soot to perform optimizations on them.
    *
    * @param classDir The directory of class files to add to Soot's class path.
    */
  private def addClassDir(classDir: String): Unit = {
    Options.v().set_process_dir(List(classDir).asJava)
  }

  /**
    * Attempts to load the specified class from Soot's class path.
    *
    * @param className The name of the class to load.
    * @return The loaded class.
    */
  private def loadClassFromClassPath(className: String): SootClass = {
    Scene.v().loadClassAndSupport(className)
    Scene.v().loadNecessaryClasses()

    Scene.v().getSootClass(className)
  }

  /**
    * Writes the given class to the specified class file path.
    *
    * @param sootClass The class to write to the file.
    * @param fileName The file path to write the class file to.
    */
  private def writeToClassFile(sootClass: SootClass, fileName: String): Unit = {
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
  private def jimpleToShimple(jimpleClass: SootClass): Unit = {
    for (method <- jimpleClass.getMethods.asScala) {
      val jimpleBody = method.retrieveActiveBody()

      val shimpleBody = Shimple.v().newBody(jimpleBody)
      shimpleBody.addAllTagsOf(jimpleBody)

      method.setActiveBody(shimpleBody)
    }
  }

  /**
    * Converts the method bodies of the given SootClass from Shimple to Jimple. This works by mutating the object in
    * place.
    *
    * @param shimpleClass The class to convert from Shimple to Jimple.
    */
  private def shimpleToJimple(shimpleClass: SootClass): Unit = {
    for (method <- shimpleClass.getMethods.asScala) {
      val shimpleBody = method.retrieveActiveBody().asInstanceOf[ShimpleBody]

      method.setActiveBody(shimpleBody.toJimpleBody)
    }
  }
}
