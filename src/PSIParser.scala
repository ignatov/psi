package psic

import util.parsing.combinator._

/*
 * author: ignatov
 */

class PSIParser extends JavaTokenParsers {
  val NullType = Type("null")

  def r: Parser[Type] = ("bool" | "nat" | "int" | "string" | "real") ^^ {x => Type(x)}

  def attributes: Parser[List[Attribute]] = r ~ repsep(ident, ",") ^^ { //todo: <s>
    case t ~ lst => lst.map(x => Attribute(x, t))
  }

  def A: Parser[List[Attribute]] = repsep(attributes, ";") ^^ {lst => lst.foldLeft(List[Attribute]()) {_ ::: _}}

  def P: Parser[Any] = ("P" ~> ident) ~ ("{" ~> repsep(R, ";") <~ "}") ^^ {
    case name ~ relations => Package(name, relations)
  }

  def R: Parser[ExprTree] = S //todo

  def S: Parser[Scheme] = ("S" ~> ident) ~ ("{" ~> A) ~ ("|" ~> repsep(F, ";") <~ "}") ^^ {
    case name ~ attributes ~ fls => Scheme(name, attributes, fls)
  }

  def F: Parser[FL] = (ident <~ "<-") ~ Y ^^ {case result ~ expr => FL(result, expr)}

  def Y: Parser[ExprTree] = (wholeNumber ^^ {x => Number(x.toInt)}
          | ident ^^ {x => Expr(x)}
          | "(" ~> X <~ ")")

  def X: Parser[ExprTree] = (Y ~ rep(op ~ Y)) ^^ {
    case a ~ lst => (a /: lst) {
      case (x, op ~ y) => Operator(x, y, op)
    }
  }

  //  def V: Parser[Any] = ("{" ~> ((A <~ "|") ~ repsep(F, ";")) <~ "}") | repsep(F, ";")

  //  def T: Parser[Any] = repsep(wholeNumber, f)

  //  def n: Parser[Any] = ident // todo

  //  def s: Parser[Any] = ident

  def op: Parser[String] = ("+" | "-" | "*" | "/" | "==" | "<" | ">" | "<>" | "<=" | ">=")
}