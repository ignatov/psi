package psic

import scala.util.parsing.combinator._

/*
 * author: ignatov
 */

class PSIParser extends JavaTokenParsers {
//  def block: Parser[Block] = rep(valdef) ~ expr ^^ {case lst ~ e => Block(lst, e)}
//
//  def expr: Parser[ExprTree] = ("if" ~> ("(" ~> expr <~ ")") ~ block <~ "else") ~ block ^^ {
//    case e ~ b1 ~ b2 => IfExpr(e, b1, b2)
//  } | expr2
//
//  def expr2: Parser[ExprTree] = (term ~ rep(("+" | "-") ~ term)) ^^ {
//    case a ~ lst => (a /: lst) {
//      case (x, "+" ~ y) => Operator(x, y, _ + _)
//      case (x, "-" ~ y) => Operator(x, y, _ - _)
//    }
//  }
//
//  def term: Parser[ExprTree] = (factor ~ rep(("*" | "/") ~ factor)) ^^ {
//    case a ~ lst => (a /: lst) {
//      case (x, "*" ~ y) => Operator(x, y, _ * _)
//      case (x, "/" ~ y) => Operator(x, y, _ / _)
//    }
//  }
//
//  def factor: Parser[ExprTree] = wholeNumber ^^ {x => Number(x.toInt)} |
//          "(" ~> expr <~ ")" | valOrFuncall
//
//  def valOrFuncall = valOrFun ~ opt("(" ~> repsep(expr, ",") <~ ")") ^^ {
//    case expr ~ Some(args) => FunCall(expr, args)
//    case expr ~ None => expr
//  }
//
//  def valOrFun = "(" ~> expr <~ ")" |
//          ident ^^ {Variable(_)} |
//          funliteral
//
//  def funliteral: Parser[ExprTree] = ("{" ~> repsep(ident, ",") <~ "=>") ~ block <~ "}" ^^ {
//    case params ~ block => Function(params, block)
//  }
//
//  def funcall: Parser[ExprTree] = expr ~ ("(" ~> repsep(expr, ",") <~ ")") ^^ {
//    case fun ~ args => FunCall(fun, args)
//  }
//
//  def expr: Parser[Any]
  
  def pack: Parser[Package] = ("P" ~> ident) ~ ("{" ~> rep(relation) <~ "}") ^^ {
    case name ~ lst => Package(name, lst)
  }

  def relation: Parser[Relation] = ("S" ~> ident) ~ ("{" ~> rep(valdef) <~ "}") ^^ {
    case name ~ lst => Relation(name, lst)
  }

  def valdef: Parser[ExprTree] = r ~ (ident <~ ";") ^^ {
    case expr ~ name => Valdef(name, expr)
  }
//
//  def fundef: Parser[Definition] = ("def" ~> ident <~ "=") ~ (funliteral <~ ";") ^^ {
//    case name ~ expr => Fundef(name, expr)
//  }

  // primitive type
  def r: Parser[ExprTree] = ("bool" | "nat" | "int" | "string" | "real") ^^ {a => Type(a)}
}