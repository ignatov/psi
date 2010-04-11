package psi.generator

import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers
import psi.compiler.metamodel.{Q, P, Converter}
import psi.compiler.parser.PSIParser
import psi.synthesizer.Prover
import psi.gererator.CLangGenerator

/**
 * User: ignatov
 * Date: 11.03.2010
 */

class CLanguageGeneratorTest extends Spec with ShouldMatchers {
  def createPrograms(pack: P): String = {
    val sb = new StringBuilder()
    for (val relation <- pack.relations.values) {
      relation match {
        case task: Q => sb.append(
          new CLangGenerator generate (new Prover doProof (pack, task))
          )
        case _ =>
      }
    }
    sb.toString
  }

  describe("A generator") {
    describe("(when program without cases)") {
      val input: String = """
        P pack {
          S Simple {
            int a, b, c, d
            |
            a <- b + c;
            b <- c * 2;
            d <- a + b
          };
          Q find_a {on Simple in c out a}
        }
      """
      val parseResult = PSIParser.parse(PSIParser.P, input)
      val result: P = Converter convert parseResult.get

      it("should generate C source") {
        createPrograms(result) should be(
          "#include<stdlib.h>\r\n" +
            "\r\n" +
            "\r\n" +
            "\r\n" +
            "int find_a(int c) {\r\n" +
            "  int b;\r\n" +
            "  int a;\r\n" +
            "\r\n" +
            "  b = (c * 2);\r\n" +
            "  a = (b + c);\r\n" +
            "  return a;\r\n" +
            "}\r\n" +
            "\r\n" +
            "int main(int argc, char *argv[]) {\r\n" +
            "  int a;\r\n" +
            "  a = find_a(int c);\r\n" +
            "  return 0;\r\n" +
            "}\r\n")
      }
    }
    describe("(when can't reacher attribute)") {
      val input: String = """
        P pack {
          S Simple {
            int a, b, c, d
            |
            a <- b + c;
            b <- c * 2;
            d <- a + b
          };
          Q find_a {on Simple in b out a}
        }
      """
      val parseResult = PSIParser.parse(PSIParser.P, input)
      val result: P = Converter convert parseResult.get
      it("failed") {
        createPrograms(result) should be(
          "#include<stdlib.h>\r\n" +
            "\r\n" +
            "\r\n" +
            "\r\n" +
            "int failed(int b) {\r\n" +
            "\r\n" +
            "  return a;\r\n" +
            "}\r\n" +
            "\r\n" +
            "int main(int argc, char *argv[]) {\r\n" +
            "  int a;\r\n" +
            "  a = failed(int b);\r\n" +
            "  return 0;\r\n" +
            "}\r\n")
      }

    }
  }
}