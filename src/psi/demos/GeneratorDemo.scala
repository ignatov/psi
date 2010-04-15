package psi.demos

import java.io.{File, FileWriter}
import compat.Platform.EOL

/**
 * User: ignatov
 * Date: 15.04.2010
 */

object GeneratorDemo {
  def main(args: Array[String]): Unit = {
    if (args.length != 1)
      return Console.err.println("Please enter filename")

    val dir: File = new File(args(0))

    if (!dir.isDirectory)
      return Console.err.println(dir + "is not a directory")

    new File("out").mkdir

    for (file <- dir.listFiles) {
      val out = new FileWriter("out/" + file.getName.replace(".psi", ".c"))
      out.write(CLangGeneratorDemo.generate(file) mkString (EOL))
      out.close
    }
  }
}