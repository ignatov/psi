package psic

/*
 * author: ignatov
 */

class ExprTree
case class Package(name: String, lst: List[ExprTree]) extends ExprTree
case class Scheme(name: String, attributes: List[Attribute], fls: List[FL]) extends ExprTree
case class Type(name: String) extends ExprTree
case class Attribute(name: String, t: Type)
case class FL(name: String, expr: ExprTree) extends ExprTree
case class Number(value: Int) extends ExprTree
case class Value(value: String) extends ExprTree
