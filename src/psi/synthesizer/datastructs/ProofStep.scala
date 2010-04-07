package psi.synthesizer.datastructs

import psi.compiler.metamodel.{N, F}

/**
 * User: ignatov
 * Date: 03.04.2010
 */

case class ProofStep(fl: F, reachedAttribute: N, condition: Condition) { //todo: add case statements
  override def toString = "ProofStep(" + fl + ", " + reachedAttribute.name.name + ")"
}