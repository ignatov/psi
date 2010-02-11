package pcis.metamodel

import compat.Platform.EOL
import collection.mutable.{ArrayBuffer, HashMap}

/**
 * @author: ignatov
 * Date:    04.02.2010
 */

/**
 * Base class for metamodel classes. May be need for converter or printer.
 */
abstract class Metamodel

/**
 * Package
 */
case class P(name: String, relations: Map[String, R]) extends Metamodel {
  override def toString =
    "P(" + name + EOL + "  " + (relations.toList map (x => x._1 + " -> " + x._2)).mkString(EOL + "  ") + EOL + ")"
}

/**
 * Relation
 */
trait R extends Metamodel {
  def name: String
}

/**
 * Scheme
 */
case class S(name: String, condition: G, thenBranch: V, elseBranch: V, fls: List[F], aTable: HashMap[String, A], nTable: HashMap[String, N]) extends R {
  def getA(attr: String): A = aTable(attr)
  def getN(attr: String): N = nTable(attr)
}

/**
 * Task
 */
case class Q(name: String, scheme: S, in: List[N], out: List[N]) extends R {
  override def toString = "Q(" + name + ", " + scheme.name + ", " + in + ", " + out + ")"
}

/**
 * Guard
 */
case class G(expr: X) extends Metamodel

/**
 * Attribute
 */
case class A(name: String, t: T) extends Metamodel

/**
 * Attribute occurrence
 */
case class N(name: A, surname: A, left: ArrayBuffer[F], right: ArrayBuffer[F]) extends Metamodel {
  def attrName = name.name + {if (surname != null) "." + surname.name else ""}
}

/**
 * Variant part
 */
case class V(fls: List[F], aTable: HashMap[String, A], nTable: HashMap[String, N]) extends Metamodel

/**
 * Functional link
 */
case class F(expr: X, res: N) extends Metamodel {
  override def toString = res.attrName + " <- " + expr.impl
}

/**
 * Expression
 */
case class X(impl: String, args: List[N]) extends Metamodel

/**
 * Type
 */
case class T(name: String) extends Metamodel