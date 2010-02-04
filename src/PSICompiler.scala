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
    val parser = new PSIParser
    var input = fromFile("input.txt") mkString
    val result = parser.parse(parser.P, input).get
    println(result)
    println(Converter run result)
  }
}