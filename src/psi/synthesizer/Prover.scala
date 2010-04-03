package psi.synthesizer

import datastructs.{ProofStep, Procedure}
import collection.mutable.ArrayBuffer
import psi.compiler.metamodel.{F, N, Q}

/**
 * User: ignatov
 * Date: 03.04.2010
 */

class Prover {
  var reached = new ArrayBuffer[N]()
  var unreached = new ArrayBuffer[N]()
  var functions = new ArrayBuffer[F]()

  def doProof(task: Q): Procedure = {
    val input = task.in
    val output = task.out

    var proofSteps = new ArrayBuffer[ProofStep]()

    unreached appendAll output

    for (a <- input)
      process(a)

    while (unreached.length > 0) {
      if (functions.length == 0)
        return new Procedure("failed", input, output, proofSteps.toList)

      val f = functions remove 0
      val result = f.res

      process(result)

      proofSteps append (new ProofStep(f, result))
    }

    new Procedure(task.name, input, output, proofSteps.toList)
  }

  def process(a: N): Unit = {
    reached append a

    for (f: F <- a.right)
      if (reached.toList.union(f.expr.args).length == reached.length)
        functions append f

    for (f: F <- a.left)
      if (functions.contains(f))
        functions.remove(functions indexOf f)

    if (unreached contains a)
      unreached remove (unreached indexOf a)
  }
}