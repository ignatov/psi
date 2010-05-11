package psi.demos

import java.io.File
import psi.compiler.metamodel.Converter
import psi.compiler.metamodel.datastructs.P
import psi.compiler.parser.PSIParser
import scala.io.Source.fromFile

/**
 * User: ignatov
 * Date: 04.02.2010
 */

object PsiCompiler {
  def main(args: Array[String]): Unit = {
    if (args.length != 1)
      Console.err.println("Please enter filename")
    else {
      val result: Option[P] = compile(new File(args(0)))
      result match {
        case None =>
        case Some(p) => println(p)
      }
    }
  }

  def compile(file: File): Option[P] = {
    val parseResult = PSIParser.parse(fromFile(file) mkString)
    if (!parseResult.successful) {
      Console.err.println("In file: " + file.getName)
      Console.err.println(parseResult)
      return None
    }
    Some(new Converter convert parseResult.get)
  }
}