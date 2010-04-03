package psi

import psi.compiler.parser.PSIParser
import scala.io.Source.fromFile
import psi.compiler.metamodel.{Q, P, Converter}
import psi.synthesizer.Prover

/**
 * User: ignatov
 * Date: 03.04.2010
 */

object PSIDemo {
  def main(args: Array[String]): Unit = {
    if (args.length == 1) {
      val parseResult = PSIParser.parse(PSIParser.P, fromFile(args(0)) mkString)
      if (!parseResult.successful)
        println(parseResult)
      else {
        val pack: P = Converter convert parseResult.get

        for (val relation <- pack.relations.values) {
          relation match {
            case task: Q => println(new Prover doProof task)
            case _ =>
          }
        }
      }
    }
    else
      Console.err.println("Please enter filename")
  }
}