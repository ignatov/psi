package psi.demos

import java.io.{File, FileWriter}
import compat.Platform.EOL
import java.lang.String

/**
 * User: ignatov
 * Date: 15.04.2010
 */

/**
 * Demo program:
 *   - reads psi source -> create metamodel -> create proof -> generate C source
 *   - puts all results to `out` directory
 */
object PsiDemo {
  def main(args: Array[String]): Unit = {
    if (args.length != 1)
      return Console.err.println("Please enter filename")

    val dir: File = new File(args(0))

    if (!dir.isDirectory)
      return Console.err.println(dir + "is not a directory")

    val outDir: String = "out"

    new File(outDir).mkdir

    for (file <- dir.listFiles; if file.getName.endsWith(".psi")) {
      val dirName = file.getName.replace(".psi", "")
      new File(outDir + "/" + dirName).mkdir

      val outC = new FileWriter("out/" + dirName + "/" + dirName + ".c")
      outC.write(CLangGeneratorDemo.generate(file) mkString EOL)
      outC.close

      val outTS = new FileWriter("out/" + dirName + "/" + dirName + ".cpp")
      outTS.write(OpenTSLangGeneratorDemo.generate(file) mkString EOL)
      outTS.close
      
      val outProof = new FileWriter("out/" + dirName + "/" + dirName + ".proof")
      outProof.write(ProverDemo.generate(file) mkString EOL)
      outProof.close

      PsiCompiler.compile(file) match {
        case None =>
        case Some(result) => {
          val outCompilation = new FileWriter("out/" + dirName + "/" + dirName + ".pack")
          outCompilation.write(result toString)
          outCompilation.close
        }
      }
    }
  }
}