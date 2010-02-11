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
  val schemeTable = new HashMap[String, S]()
  val taskTable = new HashMap[String, Q]()

  /**
   * Convert AST package to P class from metamodel
   * @param pack the package to convert
   */
  def convert(pack: Package): P = package2P(pack)

  /**
   * @param pack the package to convert
   */
  private def package2P(pack: Package): P = {
    pack.lst foreach (r => r match {
      case r: Scheme => schemeTable.put(r.name, scheme2S(r))
      case r: Task => taskTable.put(r.name, task2Q(r))
    })
    P(pack.name, Map[String, R]() ++ schemeTable ++ taskTable)
  }

  /**
   * @param scheme the scheme to convert
   */
  private def scheme2S(scheme: Scheme): S = {
    val attributeTable = new HashMap[String, A]()
    val occurrenceTable = new HashMap[String, N]()

    scheme.attributes foreach (
      a => attributeTable(a.name) = A(a.name, typeTable.getOrElseUpdate(a.t.name, T(a.t.name))))

    val g: G = getCondition(scheme.condition, attributeTable, occurrenceTable)
    val thenV = {if (scheme.condition != null) block2V(scheme.condition.positive, attributeTable) else null}
    val elseV = {if (scheme.condition != null) block2V(scheme.condition.negative, attributeTable) else null}
    val fs: List[F] = scheme.fls map (f => fl2F(f, attributeTable, occurrenceTable))

    S(scheme.name, g, thenV, elseV, fs, attributeTable, occurrenceTable)
  }

  /**
   * @param task the task to convert
   */
  private def task2Q(task: Task): Q = {
    val s = schemeTable(task.scheme)

    Q(task.name, s, task.in map (n => s.getN(n.toString)), task.out map (n => s.getN(n.toString)))
  }

  /**
   * @param ifExpr
   * @param aTable the attributes table from parent scheme
   * @param nTable the attributes occurrences table from parent scheme
   */
  private def getCondition(ifExpr: IfStatement, aTable: HashMap[String, A], nTable: HashMap[String, N]): G = ifExpr match {
    case null => null
    case i => condition2G(i.condition, aTable, nTable)
  }

  /**
   * Recursive routine witch get occurrences from expression.
   * @param expr the input expression
   */
  private def getOccurrences(expr: Expression): List[AttributeOccurrence] = expr match {
    case AttributeOccurrence(attr, sub) => List(AttributeOccurrence(attr, sub))
    case Number(_) => Nil
    case Operator(left, right, op) => getOccurrences(left) ::: getOccurrences(right)
  }

  /**
   * @param n the attribute occurrence to convert
   * @param aTable the attributes table from parent scheme
   * @param nTable the attributes occurrences table from parent scheme
   */
  private def occurrence2N(n: AttributeOccurrence, aTable: HashMap[String, A], nTable: HashMap[String, N]): N = n match {
    case AttributeOccurrence(attr, null) =>
      nTable.getOrElseUpdate(n.toString,
        N(aTable(attr), null, new ArrayBuffer[F], new ArrayBuffer[F]))
    case AttributeOccurrence(attr, sub) =>
      nTable.getOrElseUpdate(n.toString,
        N(aTable(attr), schemeTable(aTable(attr).t.name).getA(sub), new ArrayBuffer[F], new ArrayBuffer[F]))
  }

  /**
   * @param e the expression to convert
   * @param aTable the attributes table from parent scheme
   * @param nTable the attributes occurrences table from parent scheme
   */
  private def expression2X(e: Expression, aTable: HashMap[String, A], nTable: HashMap[String, N]): X =
    X(e toString, getOccurrences(e) map (n => occurrence2N(n, aTable, nTable)))

  /**
   * @param fl the functional link to convert
   * @param aTable the attributes table from parent scheme
   * @param nTable the attributes occurrences table from parent scheme
   */
  private def fl2F(fl: FL, aTable: HashMap[String, A], nTable: HashMap[String, N]): F = {
    val result = F(expression2X(fl.implementation, aTable, nTable), occurrence2N(fl.result, aTable, nTable))
    result.res.left += result
    result.expr.args foreach (n => n.right += result)
    return result
  }

  /**
   * @param condition the condition to convert
   * @param aTable the attributes table from parent scheme
   * @param nTable the attributes occurrences table from parent scheme
   */
  private def condition2G(condition: Expression, aTable: HashMap[String, A], nTable: HashMap[String, N]): G =
    G(expression2X(condition, aTable, nTable))

  /**
   * @param block the block to convert
   * @param aTable the attributes table from parent scheme
   */
  private def block2V(block: Block, aTable: HashMap[String, A]): V = {
    val aTableForV = new HashMap[String, A]()
    val nTableForV = new HashMap[String, N]()

    block.attributes foreach (
      a => aTableForV.getOrElseUpdate(a.name, A(a.name, typeTable.getOrElseUpdate(a.t.name, T(a.t.name)))))

    var as = new HashMap[String, A]()
    as ++= aTable
    as ++= aTableForV
    val fs: List[F] = block.fls map (f => fl2F(f, as, nTableForV))

    return V(fs, aTableForV, nTableForV)
  }
}