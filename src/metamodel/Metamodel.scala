package pcis.metamodel

import compat.Platform.EOL
import collection.mutable.{ArrayBuffer, HashMap}
import java.lang.String

/**
 * @author: ignatov
 * Date:    04.02.2010
 */

/**
 * Base class for metamodel classes. May be need for converter or printer.
 */
abstract class Metamodel {
  val indent: String = "  "
}

/**
 * Package
 */
case class P(name: String, relations: Map[String, R]) extends Metamodel {
  override def toString =
    "P(" + name + EOL + indent + (relations.toList map (x => x._1 + " -> " + x._2)).mkString(EOL + indent) + EOL + ")"
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

  override def toString = {
    "S(" + EOL +
      indent * 2 + "_: " + name + EOL +
      indent * 2 + "g: " + condition + EOL +
      indent * 2 + "+: " + thenBranch + EOL +
      indent * 2 + "-: " + elseBranch + EOL +
      indent * 2 + "f: " + fls.mkString(", ") + EOL +
      indent * 2 + "a: " + aTable + EOL +
      indent * 2 + "n: " + nTable + EOL +
      indent + ")"
  }
}

/**
 * Task
 */
case class Q(name: String, scheme: S, in: List[N], out: List[N]) extends R {
  override def toString = "Q(" + name + EOL +
    indent * 2 + "shm: " + scheme.name + EOL +
    indent * 2 + "in:  " + in.mkString(", ") + EOL +
    indent * 2 + "out: " + out.mkString(", ") + EOL +
    indent + ")"
}

/**
 * Guard
 */
case class G(expr: X) extends Metamodel

/**
 * Attribute
 */
case class A(name: String, t: T) extends Metamodel {
  override def toString = "A(" + name + ": " + t.name + ")"
}

/**
 * Attribute occurrence
 */
case class N(name: A, surname: A, left: ArrayBuffer[F], right: ArrayBuffer[F]) extends Metamodel {
  def attrName = name.name + {if (surname != null) "." + surname.name else ""}

  override def toString = "N(" + attrName + "; " + left.mkString(",") + "; " + right.mkString(",") + ")"
}

/**
 * Variant part
 */
case class V(fls: List[F], aTable: HashMap[String, A], nTable: HashMap[String, N]) extends Metamodel {
  override def toString = {
    "V(" + EOL +
      indent * 5 + "f: " + fls.mkString(", ") + EOL +
      indent * 5 + "a: " + aTable + EOL +
      indent * 5 + "n: " + nTable + EOL +
      indent * 4 + ")"
  }
}

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