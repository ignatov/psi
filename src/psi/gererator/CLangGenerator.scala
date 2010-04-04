package psi.gererator

import compat.Platform.EOL
import psi.synthesizer.datastructs.{ProofStep, Procedure}
import psi.compiler.metamodel.{A, N}

/**
 * User: ignatov
 * Date: 04.04.2010
 */

/**
 * Generator for C language
 */
class CLangGenerator extends Generator {
  override def generate(procedure: Procedure): String = {
    val output: A = procedure.output(0).name
    val outputDef: String = output.t.name + " " + output.name + ";"

    generateHeader +
      generateProcedure(procedure) +
      "int main(int argc, char *argv[]) {" + EOL +
      indent + outputDef + EOL +
      indent + output.name + " = " + procedure.name + "(" + inputs(procedure) + ");" + EOL +
      "}" + EOL
  }

  def generateHeader(): String = "#include<stdlib.h>" + EOL * 2

  def inputs(procedure: Procedure): String = procedure.input.map((x: N) => x.name.t.name + " " + x.name.name).mkString(", ")

  def finishSemicolon(list: List[Any]): String = {
    if (list.length == 0)
      return ""
    return ";" + EOL
  }

  def generateProcedure(procedure: Procedure): String = {
    val result: A = procedure.output(0).name

    result.t.name + " " + procedure.name + "(" + inputs(procedure) + ") {" + EOL +
      procedure.steps.map(
        (x: ProofStep) => {
          if (!procedure.input.contains(x.fl.res))
            indent + x.fl.res.name.t.name + " " + x.fl.res.attrName + ";" + EOL
          else
            ""
        }
        ).mkString("") + EOL +
      procedure.steps.map((x: ProofStep) => indent + x.fl.res.attrName + " = " + x.fl.expr.impl).mkString(";" + EOL) + finishSemicolon(procedure.steps) +
      indent + "return " + result.name + ";" + EOL +
      "}" + EOL * 2
  }
}