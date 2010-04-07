package psi.gererator

import psi.synthesizer.datastructs.Procedure

/**
 * User: ignatov
 * Date: 04.04.2010
 */

trait Generator {
  protected val indent: String = "  ";

  val typeMap: Map[String, String]

  def generate(procedure: Procedure): String
}