package psic.metamodel

import collection.mutable.HashMap
import pcis.metamodel._
import psic.parser._

/**
 * @author: ignatov
 * Date:    04.02.2010
 */

/**
 * Convert from PSI AST to PSI metamodel
 */
object Converter {
  /**
   * @param `pack' the package to be converted
   */
  def run(pack: Package): P = package2P(pack)

  private def package2P(pack: Package): P = {
    val h = new HashMap[String, R]()
    pack.lst foreach (s => h.put(s.name, relation2R(s)))
    P(pack.name, h)
  }

  private def relation2R(expr: Relation): R = expr match {
    case Scheme(name, condition, attributes, fls) => {
      val typeTable = new HashMap[String, T]() //todo: need to copy to S?
      val attributeTable = new HashMap[String, A]()

      attributes foreach (
              a => attributeTable.put(a.name, //todo: maybe putOrUpdate?
                A(a.name, typeTable.getOrElseUpdate(a.t.name, T(a.t.name)))))

      def getOccurances(expr: ExprTree): List[AttributeOccurance] = expr match {
        case AttributeOccurance(attr, sub) => List(AttributeOccurance(attr, sub))
        case Number(_) => Nil
        case Operator(left, right, op) => getOccurances(left) ::: getOccurances(right)
      }

      fls foreach (f => println(getOccurances(f.implementation)))

      S(name, null, null, null, null, attributeTable)
    }

    case Task(name, scheme, in, out) => Q(name, null, null)
    case _ => null
  }

  def occurance2N(n: AttributeOccurance): N = n match {
    case AttributeOccurance(attr, null) => null
    case AttributeOccurance(attr, sub) => null
  }
}