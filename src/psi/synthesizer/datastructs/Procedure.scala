package psi.synthesizer.datastructs

import psi.compiler.metamodel.N

/**
 * User: ignatov
 * Date: 03.04.2010
 */

case class Procedure(name: String, input: List[N], output: List[N], steps: List[ProofStep])