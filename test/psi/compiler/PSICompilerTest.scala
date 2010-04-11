package psi.compiler

import org.junit.runner.RunWith
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.junit.JUnitRunner
import org.scalatest.Spec
import metamodel.{P, Converter}
import parser.PSIParser

@RunWith(classOf[JUnitRunner])
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
  }
}