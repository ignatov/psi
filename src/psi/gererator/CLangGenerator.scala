package psi.gererator

import compat.Platform.EOL
import psi.compiler.metamodel.N
import psi.synthesizer.datastructs.{ProofStep, Procedure}

/**
 * User: ignatov
 * Date: 04.04.2010
 */

/**
 * Generator for C language
 */
class CLangGenerator extends Generator {
  override def generate(procedure: Procedure): String = {

    val inputs: String = procedure.input.map((x: N) => x.name.t.name + " " + x.name.name).mkString(";" + EOL) + ";";
    val outputs: String = procedure.output.map((x: N) => x.name.t.name + " " + x.name.name).mkString(";" + EOL) + ";";

    return "#include<stdlib.h>" + EOL + EOL +
      inputs + EOL +
      outputs + EOL + EOL +
      "void " + procedure.name + "() {" + EOL +
      procedure.steps.map((x:ProofStep) => indent + x.fl.res.attrName + " = " + x.fl.expr.impl).mkString(";" + EOL) + ";" + EOL +
      "}" + EOL + EOL +
      "int main(int argc, char *argv[]) {" + EOL +
      indent + procedure.name + "();" + EOL +
      "}" + EOL
  }
}