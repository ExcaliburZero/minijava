package minijava.optimization

import java.util

import soot.jimple.internal.ConditionExprBox
import soot.toolkits.graph.DirectedGraph
import soot.toolkits.scalar.ForwardFlowAnalysis

import collection.JavaConverters._
import scala.collection.mutable.ArrayBuffer

class NullCheckAnalysis(graph: DirectedGraph[soot.Unit]) extends ForwardFlowAnalysis[soot.Unit, util.ArrayList[soot.Unit]](graph) {
  private val unitsToRemove: ArrayBuffer[soot.Unit] = new ArrayBuffer[soot.Unit]()

  doAnalysis()

  def getUnitsToRemove(): ArrayBuffer[soot.Unit] = {
    unitsToRemove
  }

  override def flowThrough(in: util.ArrayList[soot.Unit], d: soot.Unit, out: util.ArrayList[soot.Unit]): Unit = {
    println(in)

    println(d)

    for (b <- d.getUseBoxes.asScala) {
      b match {
        case b: ConditionExprBox =>
          println(b)

          unitsToRemove.append(d)
        case _ => ()
      }
    }

    //println(d.getUseAndDefBoxes)

    println("")

    copy(in, out)
    out.add(d)
  }

  override def newInitialFlow(): util.ArrayList[soot.Unit] = {
    new util.ArrayList[soot.Unit]()
  }

  override def merge(in1: util.ArrayList[soot.Unit], in2: util.ArrayList[soot.Unit], out: util.ArrayList[soot.Unit]): Unit = {
    println("~~~~~")
    println("Merge")
    println("~~~~~")

    out.addAll(in1)
    out.addAll(in2)
  }

  override def copy(source: util.ArrayList[soot.Unit], dest: util.ArrayList[soot.Unit]): Unit = {
    dest.addAll(source)
  }
}
