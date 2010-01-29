package psic

/*
 * author: ignatov
 */

class ExprTree
case class Type(name: String) extends ExprTree
case class Package(name: String, lst: List[Relation]) extends ExprTree
case class Relation(name: String, lst: List[ExprTree]) extends ExprTree
case class VarDef(name: String, expr: ExprTree) extends ExprTree