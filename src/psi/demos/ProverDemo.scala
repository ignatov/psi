package psi.demos

import java.io.File
import scala.io.Source.fromFile
import psi.compiler.parser.PSIParser
import psi.compiler.metamodel.Converter
import psi.compiler.metamodel.datastructs.{P, Q}
import psi.synthesizer.Prover

/**
 * User: ignatov
 * Date: 03.04.2010
 */

object ProverDemo {
  def main(args: Array[String]): Unit = {
    if (args.length != 1)
      return Console.err.println("Please enter filename")

    val parseResult = PSIParser.parse(PSIParser.P, fromFile(new File(args(0))) mkString)

    if (!parseResult.successful)
      return println(parseResult)

    val pack: P = Converter convert parseResult.get
    for (relation <- pack.relations.values) {
      relation match {
        case task: Q => println(new Prover doProof (pack, task))
        case _ =>
      }
    }
  }
}