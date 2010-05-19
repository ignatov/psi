package psi.synthesizer

import datastructs.{SingleStep, ProofStep, ParallelStep}
import collection.mutable.{ArrayBuffer, Set}

/**
 * User: ignatov
 * Date: 15.05.2010
 */

object Parallelizer {
  def work(steps: List[ProofStep]): List[ProofStep] = {
    val left = Set[String]()
    val right = Set[String]()
    var tmp = List[SingleStep]()

    val result = new ArrayBuffer[ProofStep]()

    for (step: ProofStep <- steps) {
      def addStep(s: SingleStep): List[SingleStep] = {
        left += s.reachedAttribute.attrName
        right ++= s.fl.expr.args.map(x => x.attrName)
        tmp = s :: tmp
        tmp
      }
      step match {
        case s: SingleStep =>
          if (!right.contains(s.reachedAttribute.attrName) && !contains(left, s.fl.expr.args.map(x => x.attrName))) {
            tmp = addStep(s)
          }
          else {
            if (tmp.length > 1)
              result.append(new ParallelStep(tmp reverse))
            else
              result.append(tmp.head)
            left.clear()
            right.clear()
            tmp = List[SingleStep]()
            tmp = addStep(s)
          }
        case s => result.append(s)
      }
    }
    if (tmp.length == 1)
      result.appendAll(tmp)
    if (tmp.length > 1)
      result.append(new ParallelStep(tmp reverse))
    return result.toList
  }

  /**
   * Returns master.contains(slave)
   */
  private def contains[T](master: Set[T], slave: List[T]): Boolean = {
    for (e <- slave)
      if (!master.contains(e))
        return false
    true
  }
}