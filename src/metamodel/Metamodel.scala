package pcis.metamodel

import collection.mutable.{ArrayBuffer, HashMap}

/**
 * @author: ignatov
 * Date:    04.02.2010
 */

abstract class Metamodel //todo: add inheritors 

/**
 * Package
 */
case class P(name: String, features: HashMap[String, R]) {
  override def toString = {
    val separator = "\n"
    var s = new StringBuilder()
    features.foreach(r => s.append(r._1).append(" -> ").append(r._2.toString).append(separator))
    "P(" + name + separator + s.toString() + ")"
  }
}

/**
 * Relation
 */
trait R {
  def name: String

  def getA(attr: String): A
}

/**
 * Scheme
 */
case class S(name: String, condition: G, thenBranch: V, elseBranch: V, fls: List[F], aTable: HashMap[String, A], nTable: HashMap[String, N]) extends R {
  override def getA(attr: String): A = aTable(attr)
}

/**
 * Task
 */
case class Q(name: String, scheme: S, in: List[N], out: List[N]) extends R {
  override def getA(attr: String): A = null

  override def toString = "Q(" + name + ", " + scheme.name + ", " + in + ", " + out + ")"
}

/**
 * Guard
 */
case class G(expr: X)

/**
 * Attribute
 */
case class A(name: String, t: T)

/**
 * Attribute occurrence
 */
case class N(name: A, surname: A, left: ArrayBuffer[F], right: ArrayBuffer[F]) {
  def attrName = name.name + {if (surname != null) "." + surname.name else ""}
}

/**
 * Variant part
 */
case class V(fls: List[F], aTable: HashMap[String, A], nTable: HashMap[String, N])

/**
 * Functional link
 */
case class F(expr: X, res: N) {
  override def toString = res.attrName + " <- " + expr.impl
}

/**
 * Expression
 */
case class X(impl: String, args: List[N])

/**
 * Type
 */
case class T(name: String)