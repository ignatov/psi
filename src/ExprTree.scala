package psic

/*
 * author: ignatov
 */

class ExprTree
case class Type(name: String) extends ExprTree
case class Package(name: String, lst: List[ExprTree]) extends ExprTree
case class Scheme(name: String, lst: List[ExprTree]) extends ExprTree
case class AttrDef(name: String, expr: ExprTree) extends ExprTree
case class AttrAssign(name: String, expr: ExprTree) extends ExprTree