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

  /**
   * @return list of attributes reached on case branches
   */
  def processCaseStatement(scheme: S, guard: Option[G]): List[N] = {
    guard match {
      case None => Nil
      case Some(guard) =>
        if (!contains(reached.toList, guard.expr.args))
          return Nil

        val conditionStep = new ConditionStep(guard, new ArrayBuffer, new ArrayBuffer)

        def processBranch(branch: Option[V], proofSteps: ArrayBuffer[SingleStep]): List[N] = { //todo: cleanup
          branch match {
            case None => Nil
            case Some(branch) =>
              var result = List[N]()
              for (f: F <- branch.fls) { //todo: what about order?
                if (contains((reached.toList ::: result) map (_.name), f.expr.args map (_.name))) {
                  proofSteps.append(new SingleStep(f, f.res))
                  result = f.res :: result
                }
              } //todo: remove used fls
              result
          }
        }

        // life is good
        val reachedOnLeftCase = processBranch(scheme.elseBranch, conditionStep.elseSteps)
        val reachedOnRightCase = processBranch(scheme.thenBranch, conditionStep.thenSteps)

        proofSteps append conditionStep

        val intersected = (reachedOnLeftCase map ((n: N) => n.attrName)) intersect (reachedOnRightCase.toList map ((n: N) => n.attrName))

        return intersected map ((name: String) => scheme.nTable(name))
    }
  }
}