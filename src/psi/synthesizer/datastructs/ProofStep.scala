package psi.synthesizer.datastructs

import psi.compiler.metamodel.datastructs.{G, N, F}
import collection.mutable.ArrayBuffer

/**
 * User: ignatov
 * Date: 03.04.2010
 */

abstract class ProofStep

case class SingleStep(fl: F, reachedAttribute: N) extends ProofStep {
  override def toString = "ProofStep(" + fl + ", " + reachedAttribute.name.name + ")"
}

case class ConditionStep(guard: G, thenSteps: ArrayBuffer[SingleStep], elseSteps: ArrayBuffer[SingleStep]) extends ProofStep {
  override def toString = "ConditionStep(" + guard.expr.impl + ", " + thenSteps.mkString(",") + ", " + elseSteps.mkString(",") + ")"
}