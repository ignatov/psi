package psic.parser

/**
 * @author: ignatov
 * Date:    04.02.2010
 */

class ExprTree

abstract class Expression extends ExprTree
case class Number(value: Int) extends Expression
case class AttributeOccurrence(value: String, sub: String) extends Expression
case class Operator(left: ExprTree, right: ExprTree, op: String) extends Expression

abstract case class Relation(name: String) extends ExprTree
case class Scheme(override val name: String, condition: IfExpr, attributes: List[AttributeDef], fls: List[FL]) extends Relation(name)
case class Task(override val name: String, scheme: String, in: List[ExprTree], out: List[ExprTree]) extends Relation(name)

case class Package(name: String, lst: List[Relation]) extends ExprTree
case class Block(attributes: List[AttributeDef], fls: List[FL]) extends ExprTree
case class IfExpr(condition: ExprTree, positive: Block, negative: Block) extends ExprTree
case class Type(name: String) extends ExprTree
case class AttributeDef(name: String, t: Type) extends ExprTree
case class FL(result: AttributeOccurrence, implementation: ExprTree) extends ExprTree