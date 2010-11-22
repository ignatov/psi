package psi.gererator

import collection.mutable.{ArrayBuffer, HashSet}
import compat.Platform.EOL
import psi.synthesizer.datastructs.{ConditionStep, SingleStep, ProofStep, ParallelStep, Procedure}
import psi.compiler.metamodel.datastructs.{F, S, A, N}

/**
 * User: ignatov
 * Date: 22.11.2010
 */

/**
 * Generator for OpenTS language
 */
class OpenTSLangGenerator extends Generator {
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
    val outputDef: String = getType(output.t.name) + " " + output.name + ";"

    "tfun int main(int argc, char *argv[]) {" + EOL +
      indent + outputDef + EOL +
      indent + output.name + " = " + procedure.name + "(" + inputs(procedure) + ");" + EOL +
      indent + "return 0;" + EOL +
      "}" + EOL
  }

  private def generateHeader(): String = {
    "#include <stdio.h>" + EOL + "#include <stdlib.h>" + EOL * 2
  }

  private def generateStructures(procedure: Procedure): String = {
    def addAttributeType(attributeOccurrence: N): Unit = {
      val attribute: A = attributeOccurrence.name
      val typeName: String = attribute.t.name
      if (!typeMap.contains(typeName))
        schemesToGenerate += typeName
    }

    for (step: ProofStep <- procedure.steps) {
      def addType(step: SingleStep): Unit = {
        addAttributeType(step.reachedAttribute)
        addAttributeType(step.fl.res)
        step.fl.expr.args.map(addAttributeType)
      }
      step match {
        case s: SingleStep => addType(s)
        case p: ParallelStep => p.steps.foreach(addType)
        case c: ConditionStep => c.thenSteps.foreach(addType); c.elseSteps.foreach(addType)
      }
    }

    def scheme2Structure(schemeName: String): String = {
      val relation = procedure.pack.relations(schemeName)
      relation match {
        case scheme: S =>
          "typedef struct {" + EOL +
            scheme.aTable.values.map(
              (a: A) => {indent + getType(a.t.name) + " " + a.name}
              ).mkString(";" + EOL) + finishSemicolon(scheme.aTable.values.toList) + EOL +
            "} " + scheme.name + ";" + EOL * 2
        case _ => ""
      }
    }

    schemesToGenerate.map(scheme2Structure).mkString("")
  }

  private def getType(typeName: String): String = {
    if (typeMap.contains(typeName))
      typeMap(typeName)
    else
      typeName
  }

  private def inputs(procedure: Procedure): String = {
    def variableForFunctionDeclaration(x: N): String = {
      val attrName = x.name.name
      getType(x.name.t.name) + " " + attrName
    }

    procedure.input.map(variableForFunctionDeclaration).distinct mkString (", ")
  }

  private def finishSemicolon(list: Seq[Any]): String = {
    if (list.length == 0)
      return ""
    return ";"
  }

  private def generateProcedure(procedure: Procedure): String = {
    val result: A = procedure.output(0).name

    val calculateFunctions: ArrayBuffer[F] = new ArrayBuffer[F];

    def generateVariableDefinitions(steps: List[ProofStep]): String = {
      def generateDef(x: SingleStep): String = {
        indent + getType(x.fl.res.name.t.name) + " " + x.fl.res.name.name + ";" + EOL
      }

      val names = new ArrayBuffer[String]
      procedure.steps.map(
        (x: ProofStep) =>
          x match {
            case x: SingleStep =>
              if (!procedure.input.contains(x.fl.res))
                names append (generateDef(x))
              else
                ""
            case x: ParallelStep =>
              x.steps.map(s => names append (generateDef(s)))
            case x: ConditionStep =>
              x.thenSteps.map((s: SingleStep) => names append (generateDef(s)))
              x.elseSteps.map((s: SingleStep) => names append (generateDef(s)))
          }
        )
      names.toList.distinct mkString ("")
    }

    def generateFunctionalLink(s: SingleStep): String = {
      s.fl.res.attrName + " = " + (s.fl.expr.impl) + ";"
    }

    def generateTFunCall(s: SingleStep): String = {
      calculateFunctions += s.fl;
      s.fl.res.attrName + " = " +  "calculate_" + s.fl.res.attrName + "(" + s.fl.expr.args.map((x: N) => x.name.name).mkString(" ,") + ")" + ";"
    }

    def generateTFun(fl: F): String = {
      def generateSignature(fl: F): String = {
        fl.expr.args.map((x: N) => getType(x.name.t.name) + " " + x.name.name).mkString(" ,")
      }
      "tfun " + "calculate_" + fl.res.attrName + "(" + generateSignature(fl) +")" + " {" + EOL +
        indent + "return " + (fl.expr.impl) + ";" + EOL +
        "}"
    }

    getType(result.t.name) + " " + procedure.name + "(" + inputs(procedure) + ") {" + EOL +
      generateVariableDefinitions(procedure.steps) +
      EOL +
      procedure.steps.map(
        (x: ProofStep) =>
          x match {
            case x: SingleStep => indent + generateFunctionalLink(x)
            case x: ParallelStep => x.steps.map(s => indent + generateTFunCall(s)).mkString(EOL)
            case x: ConditionStep =>
              indent +
                "if (" + x.guard.expr.impl + ") {" + EOL +
                x.thenSteps.map((s: SingleStep) => indent * 2 + generateFunctionalLink(s)).mkString(EOL) + EOL +
                indent + "}" + EOL +
                indent + "else {" + EOL +
                x.elseSteps.map((s: SingleStep) => indent * 2 + generateFunctionalLink(s)).mkString(EOL) + EOL +
                indent + "}" + EOL
          }
        ).mkString(EOL) +
      EOL +
      indent + "return " + result.name + ";" + EOL +
      "}" + EOL * 2 +
      calculateFunctions.toList.map((f: F) => generateTFun(f)).mkString(EOL * 2) +
      EOL * 2
  }
}