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
  val typeTable = new HashMap[String, T]()
  val relationTable = new HashMap[String, R]()

  /**
   * @param pack the package to be converted
   */
  def run(pack: Package): P = package2P(pack)

  private def package2P(pack: Package): P = {
    pack.lst foreach (s => relationTable.put(s.name, relation2R(s)))
    P(pack.name, relationTable)
  }

  private def relation2R(expr: Relation): R = expr match {
    case Scheme(name, ifExpr, attributes, fls) => {
      val attributeTable = new HashMap[String, A]()
      val occurrenceTable = new HashMap[String, N]()

      attributes foreach (
        a => attributeTable(a.name) = A(a.name, typeTable.getOrElseUpdate(a.t.name, T(a.t.name))))

      val g: G = getCondition(ifExpr, attributeTable, occurrenceTable)
      val thenV = {if (ifExpr != null) block2V(ifExpr.positive, attributeTable) else null}
      val elseV = {if (ifExpr != null) block2V(ifExpr.negative, attributeTable) else null}
      val fs: List[F] = fls map (f => fl2F(f, attributeTable, occurrenceTable))
      S(name, g, thenV, elseV, fs, attributeTable, occurrenceTable)
    }

    case task: Task => task2Q(task)
    case _ => null
  }

  private def getCondition(ifExpr: IfExpr, aTable: HashMap[String, A], nTable: HashMap[String, N]): G = ifExpr match {
    case null => null
    case i => condition2G(i.condition, aTable, nTable)
  }

  private def getOccurrences(expr: Expression): List[AttributeOccurrence] = expr match {
    case AttributeOccurrence(attr, sub) => List(AttributeOccurrence(attr, sub))
    case Number(_) => Nil
    case Operator(left, right, op) => getOccurrences(left) ::: getOccurrences(right)
  }

  private def occurrence2N(n: AttributeOccurrence, aTable: HashMap[String, A], nTable: HashMap[String, N]): N = n match {
    case AttributeOccurrence(attr, null) =>
      nTable.getOrElseUpdate(n.toString,
        N(aTable(attr), null, new ArrayBuffer[F], new ArrayBuffer[F]))
    case AttributeOccurrence(attr, sub) =>
      nTable.getOrElseUpdate(n.toString,
        N(aTable(attr), relationTable(aTable(attr).t.name).getA(sub), new ArrayBuffer[F], new ArrayBuffer[F]))
  }

  private def expression2X(e: Expression, aTable: HashMap[String, A], nTable: HashMap[String, N]): X =
    X(e toString, getOccurrences(e) map (n => occurrence2N(n, aTable, nTable)))

  private def fl2F(fl: FL, aTable: HashMap[String, A], nTable: HashMap[String, N]): F = {
    val result = F(expression2X(fl.implementation, aTable, nTable), occurrence2N(fl.result, aTable, nTable))
    result.res.left += result
    result.expr.args foreach (n => n.right += result)
    return result
  }

  private def condition2G(condition: Expression, aTable: HashMap[String, A], nTable: HashMap[String, N]): G =
    G(expression2X(condition, aTable, nTable))

  /**
   * @param block the block to convert
   * @param aTable the attributes table from parent scheme
   * @param nTable the attributes occurrences table from parent scheme
   */
  private def block2V(block: Block, aTable: HashMap[String, A]): V = {
    val aTableForV: HashMap[String, A] = new HashMap[String, A]()
    val nTableForV = new HashMap[String, N]()

    block.attributes foreach (
      a => aTableForV.getOrElseUpdate(a.name, A(a.name, typeTable.getOrElseUpdate(a.t.name, T(a.t.name)))))

    var as = new HashMap[String, A]()
    as ++= aTable
    as ++= aTableForV
    val fs: List[F] = block.fls map (f => fl2F(f, as, nTableForV))

    return V(fs, aTableForV, nTableForV)
  }

  /**
   * @param task the task to convert
   */
  private def task2Q(task: Task): Q = {
    val s = relationTable(task.scheme).asInstanceOf[S]

    Q(task.name, s, null, null)
  }
}