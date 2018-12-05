package minijava.optimization

import java.util

import soot.Local
import soot.jimple.NullConstant
import soot.jimple.internal.{ConditionExprBox, ImmediateBox}
import soot.toolkits.graph.DirectedGraph
import soot.toolkits.scalar.ForwardFlowAnalysis

import scala.collection.mutable.ArrayBuffer

class NullCheckAnalysis(graph: DirectedGraph[soot.Unit]) extends ForwardFlowAnalysis[soot.Unit, util.HashSet[Local]](graph) {
  private val unitsToRemove: ArrayBuffer[soot.Unit] = new ArrayBuffer[soot.Unit]()

  doAnalysis()

  def getUnitsToRemove(): ArrayBuffer[soot.Unit] = {
    unitsToRemove
  }

  override def flowThrough(in: util.HashSet[Local], d: soot.Unit, out: util.HashSet[Local]): Unit = {
    if (isNullCheckCondition(d)) {
      val nullCheckedObj = getNullCheckedLocal(d)

      if (in.contains(nullCheckedObj)) {
        unitsToRemove.append(d)
      }

      out.add(nullCheckedObj)
    }

    copy(in, out)
  }

  override def newInitialFlow(): util.HashSet[Local] = {
    new util.HashSet[Local]()
  }

  override def merge(in1: util.HashSet[Local], in2: util.HashSet[Local], out: util.HashSet[Local]): Unit = {
    // Intersection, since this is a must analysis
    out.addAll(in1)
    out.retainAll(in2)
  }

  override def copy(source: util.HashSet[Local], dest: util.HashSet[Local]): Unit = {
    dest.addAll(source)
  }

  private def isNullCheckCondition(unit: soot.Unit): Boolean = {
    unit.getUseAndDefBoxes.size() == 3 &&
      unit.getUseAndDefBoxes.get(2).isInstanceOf[ConditionExprBox] &&
      unit.getUseAndDefBoxes.get(1).isInstanceOf[ImmediateBox] &&
      unit.getUseAndDefBoxes.get(1).asInstanceOf[ImmediateBox].getValue.eq(NullConstant.v())
  }

  private def getNullCheckedLocal(unit: soot.Unit): Local = {
    unit.getUseAndDefBoxes.get(0).getValue.asInstanceOf[Local]
  }
}
