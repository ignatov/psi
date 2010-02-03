package psic

/*
 * author: ignatov
 */

class ExprTree
case class Package(name: String, lst: List[ExprTree]) extends ExprTree
case class Scheme(name: String, condition: IfExpr, attributes: List[AttributeDef], fls: List[FL]) extends ExprTree
case class Block(attributes: List[AttributeDef], fls: List[FL]) extends ExprTree
case class IfExpr(condition: ExprTree, positive: Block, negative: Block) extends ExprTree
case class Type(name: String) extends ExprTree
case class AttributeDef(name: String, t: Type) extends ExprTree
case class FL(name: String, expr: ExprTree) extends ExprTree
case class Number(value: Int) extends ExprTree
case class Value(value: String) extends ExprTree
case class AttributeOccurance(value: String) extends ExprTree
case class AttrWithSubAttr(attr: String, sub: String) extends ExprTree
case class Task(name: String, scheme: String, in: List[ExprTree], out: List[ExprTree]) extends ExprTree //todo
case class Operator(right: ExprTree, left: ExprTree, op: String) extends ExprTree