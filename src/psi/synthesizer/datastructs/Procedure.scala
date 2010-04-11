package psi.synthesizer.datastructs

import psi.compiler.metamodel.datastructs.{P, N}
import compat.Platform.EOL

/**
 * User: ignatov
 * Date: 03.04.2010
 */

case class Procedure(name: String, pack: P, input: List[N], output: List[N], steps: List[ProofStep]) {
  val indent: String = "  ";
  override def toString: String = {
    "Procedure(" + EOL +
      indent + "name:   " + name + EOL +
      indent + "in:     " + input.map((x: N) => x.name.name).mkString(", ") + EOL +
      indent + "out:    " + output.map((x: N) => x.name.name).mkString(", ") + EOL +
      indent + "steps:  " + steps.map(_ toString).mkString(", ") + EOL +
      ")"
  }
}