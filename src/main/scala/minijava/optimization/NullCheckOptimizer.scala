package minijava.optimization

import java.util

import soot.jimple.internal.ConditionExprBox
import soot.{Body, BodyTransformer}

class NullCheckOptimizer extends BodyTransformer {
  override def internalTransform(b: Body, phaseName: String, options: util.Map[String, String]): Unit = {
    println(phaseName)
    //println(options)
    //println(b.getUseAndDefBoxes)
    println("~~~~~~~~")

    for (box <- b.getUseAndDefBoxes.toArray()) {
      box match {
        case check: ConditionExprBox =>
          println(check)




        case _ => ()
      }
    }

    println("~~~~~~~~")

    //println(b.getUseAndDefBoxes)

    println("--------")
  }
}
