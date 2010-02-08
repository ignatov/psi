package psic.metamodel

import pcis.metamodel._
import psic.parser._
import collection.mutable.{ArrayBuffer, HashMap}

/**
 * @author: ignatov
 * Date:    04.02.2010
 */

/**
 * Convert from PSI AST to PSI metamodel
 */
object Converter {
  val relationTable = new HashMap[String, R]()

  /**
   * @param `pack' the package to be converted
   */
  def run(pack: Package): P = package2P(pack)

  private def package2P(pack: Package): P = {
    pack.lst foreach (s => relationTable.put(s.name, relation2R(s)))
    relationTable -= "#"
    P(pack.name, relationTable)
  }

  private def relation2R(expr: Relation): R = expr match {
    case Scheme(name, ifExpr, attributes, fls) => {
      val typeTable = new HashMap[String, T]() //todo: need to copy to S?
      val attributeTable = new HashMap[String, A]()
      val occurrenceTable = new HashMap[String, N]()

      attributes foreach (
        a => attributeTable(a.name) = A(a.name, typeTable.getOrElseUpdate(a.t.name, T(a.t.name))))

      relationTable("#") = S(null, null, null, null, null, attributeTable, null) //todo: hack

      S(name, getCondition(ifExpr, occurrenceTable), null, null, fls map (f => fl2F(f, occurrenceTable)), attributeTable, occurrenceTable)
    }

    case Task(name, scheme, in, out) => Q(name, null, null)
    case _ => null
  }

  private def getCondition(ifExpr: IfExpr, nTable: HashMap[String, N]): G = ifExpr match {
    case null => null
    case i => condition2G(i.condition, nTable)
  }

  private def getOccurrences(expr: Expression): List[AttributeOccurrence] = expr match {
    case AttributeOccurrence(attr, sub) => List(AttributeOccurrence(attr, sub))
    case Number(_) => Nil
    case Operator(left, right, op) => getOccurrences(left) ::: getOccurrences(right)
  }

  private def occurrence2N(n: AttributeOccurrence, nTable: HashMap[String, N]): N = n match {
    case AttributeOccurrence(attr, null) =>
      nTable.getOrElseUpdate(n.toString,
        N(relationTable("#").getA(attr), null, new ArrayBuffer[F], new ArrayBuffer[F]))
    case AttributeOccurrence(attr, sub) =>
      nTable.getOrElseUpdate(n.toString,
        N(relationTable("#").getA(attr), relationTable(relationTable("#").getA(attr).t.name).getA(sub), new ArrayBuffer[F], new ArrayBuffer[F]))
  }

  private def expression2X(e: Expression, nTable: HashMap[String, N]): X = X(e toString, getOccurrences(e) map (n => occurrence2N(n, nTable)))

  private def fl2F(fl: FL, nTable: HashMap[String, N]): F = {
    val result = F(expression2X(fl.implementation, nTable), occurrence2N(fl.result, nTable))
    result.res.left += result
    result.expr.args foreach (n => n.right += result)
    return result
  }

  private def condition2G(condition: Expression, nTable: HashMap[String, N]): G = G(expression2X(condition, nTable))

  //  private def block2V(block: Block): V = V()
}