package psic

/*
 * author: ignatov
 */

object PSICompiler {
  def spy[T](t: T) = {println(t); t}

  def lookup(name: String, symbols: List[(String, Any)]) =
    symbols.find(_._1 == name) match {
      case Some(pair) => pair._2
      case None => None
    }

  def eval(expr: ExprTree, symbols: List[(String, Any)]): Any =
    expr match {
      case Number(num) => num
      case Variable(name) => lookup(name, symbols)
      case Operator(left, right, f) => {
        eval(left, symbols) match {
          case arg1: Int =>
            eval(right, symbols) match {
              case arg2: Int =>
                f(arg1, arg2)
            }
        }
      }

      case IfExpr(cond, block1, block2) => {
        eval(cond, symbols) match {
          case result: Int =>
            if (result > 0) evalBlock(block1, symbols) else evalBlock(block2, symbols)
        }
      }

      case Funcall(fun, args) => eval(fun, symbols) match {
        case Closure(params, body, syms) =>
          evalBlock(body, params.zip(args.map(eval(_, symbols))) ::: syms)
      }

      case Function(params, body) => Closure(params, body, symbols)

      case _ => expr
    }

  def evalDef(symbols: List[(String, Any)], defn: Definition) =
    defn match {
      case Fundef(name, Function(params, body)) => {
        val cl = Closure(params, body, symbols)
        val syms = (name, cl) :: symbols
        cl.env = syms
        syms
      }
      case Valdef(name, Function(params, body)) => {(name, Closure(params, body, symbols)) :: symbols}
      case Valdef(name, expr) => {(name, eval(expr, symbols)) :: symbols}
    }

  def evalBlock(block: Block, symbols: List[(String, Any)]): Any =
    eval(block.expr, (symbols /: block.defs) {evalDef(_, _)})

  def main(args: Array[String]): Unit = {
    val parser = new SL1Parser
    val result = parser.parse(parser.block, "val sq = { x => x * x } ; sq(2)")
    println(result)
    println(evalBlock(result.get, List()))
  }
}