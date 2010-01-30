package psic

/*
 * author: ignatov
 */

class ExprTree
case class Package(name: String, lst: List[ExprTree]) extends ExprTree
case class Scheme(name: String, attributes: List[Attribute], fls: List[FL]) extends ExprTree
case class Type(name: String) extends ExprTree
case class Attribute(name: String, t: Type) extends ExprTree
case class FL(name: String, expr: ExprTree) extends ExprTree
case class Number(value: Int) extends ExprTree
case class Value(value: String) extends ExprTree
case class Expr(value: String) extends ExprTree
case class AttrWithSubAttr(attr: String, sub: String) extends ExprTree
case class Operator(right: ExprTree, left: ExprTree, op: String) extends ExprTree