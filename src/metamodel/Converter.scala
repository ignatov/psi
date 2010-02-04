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

      def getOccurrences(expr: ExprTree): List[AttributeOccurrence] = expr match {
        case AttributeOccurrence(attr, sub) => List(AttributeOccurrence(attr, sub))
        case Number(_) => Nil
        case Operator(left, right, op) => getOccurrences(left) ::: getOccurrences(right)
      }

      fls foreach (f => println(getOccurrences(f.implementation)))

      S(name, null, null, null, null, attributeTable)
    }

    case Task(name, scheme, in, out) => Q(name, null, null)
    case _ => null
  }

  def occurrence2N(n: AttributeOccurrence): N = n match {
    case AttributeOccurrence(attr, null) => null
    case AttributeOccurrence(attr, sub) => null
  }
}