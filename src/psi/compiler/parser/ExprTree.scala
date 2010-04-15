package psi.compiler.parser

/**
 * User: ignatov
 * Date: 04.02.2010
 */

/**
 * Base class for parser expressions
 */
class ExprTree

/**
 * Real expressions such as numbers, operators and attribute occurrences
 */
abstract class Expression extends ExprTree {
  override def toString(): String = this match {
    case Number(value) => value toString
    case AttributeOccurrence(value, None) => value
    case AttributeOccurrence(value, Some(sub)) => value + "." + sub
    case Operator(left, right, op) => List(left, op, right) mkString ("(", " ", ")")
  }
}

/**
 * Number representation
 */
case class Number(value: Int) extends Expression

/**
 * Attribute occurrence
 * Also support dotted names i.e. attr.sub
 */
case class AttributeOccurrence(value: String, sub: Option[String]) extends Expression

/**
 * Binary operator `op` for `left` and `right` expressions
 */
case class Operator(left: Expression, right: Expression, op: String) extends Expression

/**
 * Relation base class for named elements
 */
abstract class Relation(name: String) extends ExprTree

/**
 * Scheme representation
 */
case class Scheme(name: String, condition: Option[IfStatement], attributes: List[AttributeDef], fls: List[FL]) extends Relation(name)

/**
 * Task representation
 */
case class Task(name: String, scheme: String, in: List[AttributeOccurrence], out: List[AttributeOccurrence]) extends Relation(name)

/**
 * Package representation
 */
case class Package(name: String, lst: List[Relation]) extends ExprTree

/**
 * Block it is a scope of attributes and functional links definitions
 */
case class Block(attributes: List[AttributeDef], fls: List[FL]) extends ExprTree

/**
 * Conditional statement with positive and negative branches
 */
case class IfStatement(condition: Expression, positive: Block, negative: Block) extends ExprTree

/**
 * Functional link
 */
case class FL(result: AttributeOccurrence, implementation: Expression) extends ExprTree

/**
 * Attribute definition
 */
case class AttributeDef(name: String, t: Type) extends ExprTree

/**
 * Attribute type
 */
case class Type(name: String) extends ExprTree