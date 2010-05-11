package psi.demos

import java.io.File
import psi.compiler.parser.PSIParser
import psi.compiler.metamodel.Converter
import psi.synthesizer.Prover
import psi.compiler.metamodel.datastructs.{R, P, Q}

/**
 * User: ignatov
 * Date: 03.04.2010
 */

object ProverDemo {
  def generate(file: File): Iterable[String] = {
    val parseResult = PSIParser.parse(scala.io.Source.fromFile(file) mkString)

    if (!parseResult.successful) {
      Console.err.println("In file: " + file.getName)
      Console.err.println(parseResult)
      return Nil
    }

    val pack: P = new Converter convert parseResult.get
    pack.relations.values.map(
      (relation: R) => relation match {
        case task: Q => new Prover doProof(pack, task) toString
        case _ => ""
      }).filter((x) => x != "")
  }

  def main(args: Array[String]): Unit = {
    if (args.length != 1)
      return Console.err.println("Please enter filename")

    val file: File = new File(args(0))
    generate(file) map println
  }
}