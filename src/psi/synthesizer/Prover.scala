package psi.synthesizer

import datastructs.{ProofStep, Procedure}
import collection.mutable.ArrayBuffer
import psi.compiler.metamodel.{S, F, N, Q}

/**
 * User: ignatov
 * Date: 03.04.2010
 */

object Prover {
  var reachedList = new ArrayBuffer[N]()
  var unreachedList = new ArrayBuffer[N]()
  var G = new ArrayBuffer[F]()

  def doProof(task: Q): Procedure = {
    val input = task.in
    val output = task.out

    var proofSteps = new ArrayBuffer[ProofStep]()

    reachedList appendAll input
    unreachedList appendAll output

    for (a <- input) {
      processAttribute(task.scheme, a)
      if (unreachedList contains a)
        unreachedList remove (unreachedList indexOf a)
      proofSteps append (new ProofStep(null, a))
    }


//    while (unreachedList.length > 0) {
//
//    }

    new Procedure(task.name, input, output, proofSteps.toList)
  }

  def processAttribute(scheme: S, a: N): Unit = {
    for (f: F <- a.right) {
      if (reachedList.toList.union(f.expr.args).length == reachedList.length)
        G.append(f)
    }

    for (f: F <- a.left) {
      G.remove(G.indexOf(f))
    }
  }
}