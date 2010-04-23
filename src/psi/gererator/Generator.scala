package psi.gererator

import psi.synthesizer.datastructs.Procedure

/**
 * User: ignatov
 * Date: 04.04.2010
 */

/**
 * Trait for target languages generator
 */
trait Generator {
  protected val indent: String = "  ";

  /**
   * We need map for mapping PSI-types to real types on target language 
   */
  val typeMap: Map[String, String]

  /**
   * Main generator method: create target language source code for input procedure
   */
  def generate(procedure: Procedure): String
}