package psi.synthesizer.datastructs

import psi.compiler.metamodel.datastructs.{G, N, F}
import collection.mutable.ArrayBuffer

/**
 * User: ignatov
 * Date: 03.04.2010
 */

/**
 * Base class for proof steps
 */
abstract class ProofStep

/**
 * Linear step
 */
case class SingleStep(fl: F, reachedAttribute: N) extends ProofStep {
  override def toString = "ProofStep(" + fl + ", " + reachedAttribute.name.name + ")"
}

/**
 * Condition step with two lists of single steps
 */
case class ConditionStep(guard: G, thenSteps: ArrayBuffer[SingleStep], elseSteps: ArrayBuffer[SingleStep]) extends ProofStep {
  override def toString = "ConditionStep(" + guard.expr.impl + ", " + thenSteps.mkString(", ") + " | " + elseSteps.mkString(", ") + ")"
}