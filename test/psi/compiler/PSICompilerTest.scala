package psi.compiler

import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers
import parser.PSIParser
import metamodel.{P, Converter}

/**
 * User: ignatov
 * Date: 11.03.2010
 */

class PSICompilerTest extends Spec with ShouldMatchers {
  describe("A package") {
    describe("(when empty)") {
      val parseResult = PSIParser.parse(PSIParser.P, "P pack {}")
      val result: P = Converter convert parseResult.get

      it("should have name") {
        result.name should be("pack")
      }

      it("relations should be empty") {
        result.relations should be('empty)
      }
    }

    describe("(with scheme and task)") {
      val input: String = """
        P Simple {
          S Simple {
            int a, b, c, d
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
      val parseResult = PSIParser.parse(PSIParser.P, input)
      val result: P = Converter convert parseResult.get

      it("relations should have lenght 2") {
        result.relations.size should be(2)
      }
    }
  }
}