package psic

import scala.io.Source.fromFile
import psic.parser.PSIParser

/*
 * author: ignatov
 */

object PSICompiler {
  def main(args: Array[String]): Unit = {
    val parser = new PSIParser
    var input = fromFile("input.txt").mkString
    val result = parser.parse(parser.P, input)
    println(result)
  }
}