package psi.synthesizer.datastructs

import psi.compiler.metamodel.datastructs.G

/**
 * User: ignatov
 * Date: 07.04.2010
 */

case class Condition(guard: G, isPositive: Boolean)