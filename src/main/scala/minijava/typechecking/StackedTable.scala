package minijava.typechecking

import scala.annotation.tailrec
import scala.collection.mutable

object StackedTable {
  def apply[A, B](): StackedTable[A, B] = {
    new StackedTable(List())
  }
}

class StackedTable[A, B](prev: List[mutable.HashMap[A, B]]) {
  val stack: List[mutable.HashMap[A, B]] = mutable.HashMap[A, B]() :: prev

  def put(key: A, value: B): Boolean = {
    if (stack.head.contains(key)) {
      false
    } else {
      stack.head.put(key, value)

      true
    }
  }

  def get(key: A): Option[B] = {
    getFromStack(stack, key)
  }

  @tailrec
  private def getFromStack(cur: List[mutable.HashMap[A, B]], a: A): Option[B] = {
    cur match {
      case Nil => None
      case h :: t =>
        h.get(a) match {
          case Some(b) => Some(b)
          case None => getFromStack(t, a)
        }
    }
  }

  def newFrame(): StackedTable[A, B] = {
    new StackedTable(stack)
  }
}

