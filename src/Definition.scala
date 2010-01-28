package psic

/*
 * author: ignatov
 */

class Definition
case class Valdef(name: String, expr: ExprTree) extends Definition
case class Fundef(name: String, expr: ExprTree) extends Definition