package psic

/*
 * author: ignatov
 */

class ExprTree
case class Type(name: String) extends ExprTree
case class Package(name: String, lst: List[Relation]) extends ExprTree
case class Relation(name: String, lst: List[Any]) extends ExprTree
case class Valdef(name: String, expr: ExprTree) extends ExprTree