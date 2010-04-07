package psi.gererator

import psi.compiler.metamodel.{S, A, N}
import compat.Platform.EOL
import psi.synthesizer.datastructs.{ProofStep, Procedure}
import collection.mutable.HashSet

/**
 * User: ignatov
 * Date: 04.04.2010
 */

/**
 * Generator for C language
 */
class CLangGenerator extends Generator {
  private var schemesToGenerate = new HashSet[String]()

  override val typeMap = Map(
    "bool" -> "int",
    "nat" -> "int",
    "int" -> "int",
    "string" -> "char*",
    "real" -> "double"
    )

  override def generate(procedure: Procedure): String = {
    generateHeader() +
      generateStructures(procedure) +
      generateProcedure(procedure) +
      generateMain(procedure)
  }

  private def generateMain(procedure: Procedure): String = {
    val output: A = procedure.output(0).name
    val outputDef: String = typeMap(output.t.name) + " " + output.name + ";"

    "int main(int argc, char *argv[]) {" + EOL +
      indent + outputDef + EOL +
      indent + output.name + " = " + procedure.name + "(" + inputs(procedure) + ");" + EOL +
      indent + "return 0;" + EOL +
      "}" + EOL
  }

  private def generateHeader(): String = {
    "#include<stdlib.h>" + EOL * 2
  }

  private def generateStructures(procedure: Procedure): String = {
    def addAttributeType(attributeOccurrence: N): Unit = {
      val attribute: A = attributeOccurrence.name
      val typeName: String = attribute.t.name
      if (!typeMap.keys.contains(typeName))
        schemesToGenerate += typeName
    }

    for (step: ProofStep <- procedure.steps) {
      addAttributeType(step.reachedAttribute)
      addAttributeType(step.fl.res)
      step.fl.expr.args.map(addAttributeType)
    }

    def scheme2Structure(schemeName: String): String = {
      val relation = procedure.pack.relations(schemeName)
      relation match {
        case scheme: S => {
          "typedef struct {" + EOL +
            scheme.aTable.values.map(
              (a: A) => {indent + typeMap(a.t.name) + " " + a.name}
              ).mkString(";" + EOL) + finishSemicolon(scheme.aTable.values.toList) +
            "} " + scheme.name + ";"
        }
        case _ => ""
      }
    }

    schemesToGenerate.map(scheme2Structure).mkString(EOL) + EOL * 2
  }

  private def inputs(procedure: Procedure): String = {
    def variableForFunctionDeclaration(x: N): String = {
      val attrName = x.name.name
      val typeName = {
        val t: String = x.name.t.name
        if (typeMap.contains(t))
          typeMap(t)
        else
          t
      }

      typeName + " " + attrName
    }

    procedure.input.map(variableForFunctionDeclaration).removeDuplicates mkString(", ")
  }

  private def finishSemicolon(list: Seq[Any]): String = {
    if (list.length == 0)
      return ""
    return ";" + EOL
  }

  private def generateProcedure(procedure: Procedure): String = {
    val result: A = procedure.output(0).name

    typeMap(result.t.name) + " " + procedure.name + "(" + inputs(procedure) + ") {" + EOL +
      procedure.steps.map(
        (x: ProofStep) => {
          if (!procedure.input.contains(x.fl.res))
            indent + typeMap(x.fl.res.name.t.name) + " " + x.fl.res.attrName + ";" + EOL
          else
            ""
        }
        ).mkString("") + EOL +
      procedure.steps.map((x: ProofStep) => indent + x.fl.res.attrName + " = " + (x.fl.expr.impl)).mkString(";" + EOL) + finishSemicolon(procedure.steps) +
      indent + "return " + result.name + ";" + EOL +
      "}" + EOL * 2
  }
}