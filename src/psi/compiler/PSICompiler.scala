package psi.compiler

import metamodel.Converter
import parser.PSIParser
import scala.io.Source.fromFile

/**
 * User: ignatov
 * Date: 04.02.2010
 */

object PSICompiler {
  def main(args: Array[String]): Unit = {
    if (args.length == 1) {
      val parseResult = PSIParser.parse(PSIParser.P, fromFile(args(0)) mkString)
      if (!parseResult.successful)
        println(parseResult)
      else
        println(Converter convert parseResult.get)
    }
    else
      Console.err.println("Please enter filename")
  }
}