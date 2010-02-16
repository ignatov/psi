package psic

import scala.io.Source.fromFile
import psic.parser.PSIParser
import psic.metamodel.Converter

/**
 * @author: ignatov
 * Date:    04.02.2010
 */

object PSICompiler {
  def main(args: Array[String]): Unit = {
    if (args.length == 1) {
      val parser = new PSIParser
      var input = fromFile(args(0)) mkString
      val result = parser.parse(parser.P, input).get
      println(Converter convert result)
    }
    else {
      Console.err.println("Please enter filename")
    }
  }
}