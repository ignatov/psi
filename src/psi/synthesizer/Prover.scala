package psi.synthesizer

import datastructs.{ConditionStep, SingleStep, ProofStep, Procedure}
import psi.compiler.metamodel.datastructs._
import collection.mutable.ArrayBuffer
/**
 * User: ignatov
 * Date: 03.04.2010
 */

class Prover {
  var reached = new ArrayBuffer[N]
  var unreached = new ArrayBuffer[N]
  var functions = new ArrayBuffer[F]
  var proofSteps = new ArrayBuffer[ProofStep]

  def doProof(pack: P, task: Q): Procedure = {
    val input = task.in
    val output = task.out

    unreached appendAll output

    for (a <- input)
      process(a)

    while (unreached.length > 0) {
      if (functions.length == 0)
        for (a <- processCaseStatement(task.scheme, task.scheme.condition))
          process(a)

      if (functions.length == 0)
        return new Procedure("failed", pack, input, output, proofSteps.toList)

      val f = functions remove 0
      val result = f.res

      process(result)

      proofSteps append (new SingleStep(f, result))
    }

    new Procedure(task.name, pack, input, output, proofSteps.toList)
  }

  private def contains(master: List[Any], slave: List[Any]): Boolean = {
    for (e <- slave)
      if (!master.contains(e))
        return false
    true
  }

  def process(a: N): Unit = {
    reached append a

    for (f: F <- a.right)
      if (contains(reached toList, f.expr.args))
        functions append f

    for (f: F <- a.left)
      if (functions contains f)
        functions remove (functions indexOf f)

    if (unreached contains a)
      unreached remove (unreached indexOf a)
  }

  def processCaseStatement(scheme: S, guard: G): List[N] = {
    if (guard == null || !contains(reached.toList, guard.expr.args))
      return Nil

    // life is good
    val reachedOnLeftCase = new ArrayBuffer[N]
    val reachedOnRightCase = new ArrayBuffer[N]
    val leftFunctions = new ArrayBuffer[F]
    val rightFunctions = new ArrayBuffer[F]
    val conditionStep = new ConditionStep(guard, new ArrayBuffer, new ArrayBuffer)

    for (f: F <- scheme.thenBranch.fls) { //todo: what about order?
      if (contains((reached.toList ::: reachedOnRightCase.toList) map (_.name), f.expr.args map (_.name))) {
        rightFunctions append f
        reachedOnRightCase append f.res
        conditionStep.thenSteps.append(new SingleStep(f, f.res))
      }
    } //todo: remove used fls

    for (f: F <- scheme.elseBranch.fls) {
      if (contains((reached.toList ::: reachedOnLeftCase.toList) map (_.name), f.expr.args map (_.name))) {
        leftFunctions append f
        reachedOnLeftCase append f.res
        conditionStep.elseSteps.append(new SingleStep(f, f.res))
      }
    }

    val intersected = ((reachedOnLeftCase.toList) map ((n: N) => n.attrName)) intersect ((reachedOnRightCase.toList) map ((n: N) => n.attrName))

//    for (f <- rightFunctions if intersected contains f.res.attrName)
//      proofSteps append new SingleStep(f, f.res)
//
//    for (f <- leftFunctions if intersected contains f.res.attrName)
//      proofSteps append new SingleStep(f, f.res)

    proofSteps append conditionStep

    return intersected map ((name: String) => scheme.nTable(name))
  }
}