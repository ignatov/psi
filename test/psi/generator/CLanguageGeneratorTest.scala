package psi.generator

import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers
import psi.compiler.metamodel.Converter
import psi.compiler.metamodel.datastructs.{P, Q}
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

  def getResult(input: String): P = {
    val parseResult = PSIParser.parse(PSIParser.P, input)
    Converter convert parseResult.get
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

      val result = getResult(input)

      it("should generate C source") {
        createPrograms(result) should be(
          "#include<stdlib.h>\r\n" +
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

      val result = getResult(input)

      it("failed") {
        createPrograms(result) should be(
          "#include<stdlib.h>\r\n" +
            "\r\n" +
            "int failed(int b) {\r\n" +
            "\r\n" +
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

    describe("(when program with cases)") {
      val input: String = """
        P pack {
          S Simple {
            int a, b, c
            |
            a <- b + c
          }
          if c > 0 then
            b <- c * 2
          else
            b <- c * 3
          fi;

          Q find_a {on Simple in c out a}
        }
      """

      val result = getResult(input)

      it("should generate C source") {
        createPrograms(result) should be(
          "#include<stdlib.h>\r\n" +
            "\r\n" +
            "int find_a(int c) {\r\n" +
            "  int b;\r\n" +
            "  int a;\r\n" +
            "\r\n" +
            "  if ((c > 0)) {\r\n" +
            "    b = (c * 2);\r\n" +
            "  }\r\n" +
            "  else {\r\n" +
            "    b = (c * 3);\r\n" +
            "  }\r\n" +
            "\r\n" +
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

    describe("(when program with local attributes in cases)") {
      val input: String = """
        P pack {
          S Simple {
            int a, b, c, i
            |
            a <- b + c
          }
          if c > 0 then
            i <- c * c;
            b <- c * 2 * i
          else
            b <- c * 3
          fi;

          Q find_a {on Simple in c out a}
        }
      """

      val result = getResult(input)

      it("should generate C source") {
        createPrograms(result) should be(
          "#include<stdlib.h>\r\n" +
            "\r\n" +
            "int find_a(int c) {\r\n" +
            "  int i;\r\n" +
            "  int b;\r\n" +
            "  int a;\r\n" +
            "\r\n" +
            "  if ((c > 0)) {\r\n" +
            "    i = (c * c);\r\n" +
            "    b = ((c * 2) * i);\r\n" +
            "  }\r\n" +
            "  else {\r\n" +
            "    b = (c * 3);\r\n" +
            "  }\r\n" +
            "\r\n" +
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

    describe("(when program with local case conditions)") {
      val input: String = """
        P pack {
          S Simple {
            int a, b, c
            |
            a <- b + c
          }
          if c > 0 then {
            int i
            |
            i <- c * c;
            b <- c * 2 * i
          }
          else
            b <- c * 3
          fi;

          Q find_a {on Simple in c out a}
        }
      """

      val result = getResult(input)

      it("should generate C source") {
        createPrograms(result) should be(
          "#include<stdlib.h>\r\n" +
            "\r\n" +
            "int find_a(int c) {\r\n" +
            "  int i;\r\n" +
            "  int b;\r\n" +
            "  int a;\r\n" +
            "\r\n" +
            "  if ((c > 0)) {\r\n" +
            "    i = (c * c);\r\n" +
            "    b = ((c * 2) * i);\r\n" +
            "  }\r\n" +
            "  else {\r\n" +
            "    b = (c * 3);\r\n" +
            "  }\r\n" +
            "\r\n" +
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
  }
}