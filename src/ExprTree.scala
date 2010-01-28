package psic

/*
 * author: ignatov
 */

class ExprTree
case class Block(defs: List[Definition], expr: ExprTree)
case class Number(value: Int) extends ExprTree
case class Variable(name: String) extends ExprTree
case class Operator(left: ExprTree, right: ExprTree, f: (Int, Int) => Int) extends ExprTree
case class Function(params: List[String], body: Block) extends ExprTree
case class IfExpr(cond: ExprTree, thenBlock: Block, elseBlock: Block) extends ExprTree
case class FunCall(fun: ExprTree, args: List[ExprTree]) extends ExprTree
case class Closure(params: List[String], body: Block, var env: List[(String, Any)]) extends ExprTree {
  override def toString = "Closure(" + params + "," + body + ")"
}