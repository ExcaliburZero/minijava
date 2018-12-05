package minijava.optimization

import java.util

import soot.Local
import soot.jimple.internal._
import soot.jimple.{NullConstant, ThisRef}
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
    } else if (isSimpleAssignment(d)) {
      val local = getVarAssignedInSimpleAssignment(d)

      if (isAssignmentToNewObject(d) || isAssignedValueKnownNonNull(d, in)) {
        out.add(local)
      }
    } else if (isAssignmentToThis(d)) {
      val local = getVarAssignedToThis(d)
      out.add(local)
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

  private def isSimpleAssignment(unit: soot.Unit): Boolean = {
    unit.getUseAndDefBoxes.size() == 2 &&
      unit.getUseAndDefBoxes.get(0).isInstanceOf[VariableBox] &&
      unit.getUseAndDefBoxes.get(1).isInstanceOf[RValueBox]
  }

  private def getVarAssignedInSimpleAssignment(unit: soot.Unit): Local = {
    unit.getUseAndDefBoxes.get(0).getValue.asInstanceOf[Local]
  }

  private def isAssignedValueKnownNonNull(unit: soot.Unit, in: util.HashSet[Local]): Boolean = {
    isAssignmentToPrevVariable(unit) && in.contains(getAssignedVar(unit))
  }

  private def isAssignmentToPrevVariable(unit: soot.Unit): Boolean = {
    unit.getUseAndDefBoxes.get(1).getValue.isInstanceOf[JimpleLocal]
  }

  private def getAssignedVar(unit: soot.Unit): Local = {
    unit.getUseAndDefBoxes.get(1).getValue.asInstanceOf[Local]
  }

  private def isAssignmentToNewObject(unit: soot.Unit): Boolean = {
    unit.getUseAndDefBoxes.get(1).getValue.isInstanceOf[JNewExpr]
  }

  private def isAssignmentToThis(unit: soot.Unit): Boolean = {
    unit.getUseAndDefBoxes.size() == 2 &&
      unit.getUseAndDefBoxes.get(0).isInstanceOf[JimpleLocalBox] &&
      unit.getUseAndDefBoxes.get(1).isInstanceOf[IdentityRefBox] &&
      unit.getUseAndDefBoxes.get(1).getValue.isInstanceOf[ThisRef]
  }

  private def getVarAssignedToThis(unit: soot.Unit): Local = {
    unit.getUseAndDefBoxes.get(0).getValue.asInstanceOf[Local]
  }
}
