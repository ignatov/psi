package psi.demos

import java.io.File
import psi.synthesizer.Prover
import psi.compiler.metamodel.Converter
import psi.compiler.metamodel.datastructs.{P, Q}
import psi.compiler.parser.PSIParser
import psi.gererator.CLangGenerator

/**
 * User: ignatov
 * Date: 04.04.2010
 */

object CLangGeneratorDemo {
  def main(args: Array[String]): Unit = {
    if (args.length != 1)
      return Console.err.println("Please enter filename")

    val parseResult = PSIParser.parse(PSIParser.P, scala.io.Source.fromFile(new File(args(0))) mkString)

    if (!parseResult.successful)
      return println(parseResult)

    val pack: P = Converter convert parseResult.get
    for (relation <- pack.relations.values) {
      relation match {
        case task: Q => println((new CLangGenerator generate (new Prover doProof (pack, task))) + "-" * 20)
        case _ =>
      }
    }
  }
}