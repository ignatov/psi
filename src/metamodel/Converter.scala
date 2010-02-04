package metamodel

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
  def run(pack: Package): P = {
    val h = new HashMap[String, R]()
    pack.lst foreach(s => h.put(s.name, relation2R(s)))
    P(pack.name, h)
  }

  def relation2R(expr: ExprTree): R = expr match {
    case Scheme(name, condition, attributes, fls) => S(name, null, null, null, null, null)
    case Task(name, scheme, in, out) => Q(name, null, null)
    case _ => null
  }
}