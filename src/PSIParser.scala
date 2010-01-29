package psic

import util.parsing.combinator._

/*
 * author: ignatov
 */

class PSIParser extends JavaTokenParsers {
  def r: Parser[Type] = ("bool" | "nat" | "int" | "string" | "real") ^^ {x => Type(x)}

  def attributes: Parser[List[Attribute]] = r ~ repsep(ident, ",") ^^ { //todo: <s>
    case t ~ lst => lst.map(x => Attribute(x, t))
  }

  def A: Parser[List[Attribute]] = repsep(attributes, ";") ^^ {lst => lst.foldLeft(List[Attribute]()){_:::_}}

  def P: Parser[Any] = ("P" ~> ident) ~ ("{" ~> repsep(R, ";") <~ "}") ^^ {
    case name ~ relations => Package(name, relations)
  }

  def R: Parser[ExprTree] = S //todo

  def S: Parser[Scheme] = ("S" ~> ident) ~ ("{" ~> A) ~ ("|" ~> repsep(F, ";") <~ "}") ^^ {
    case name ~ attributes ~ fls => Scheme(name, attributes, fls)
  }

  def F: Parser[FL] = (ident <~ "<-") ~ Y ^^ {case name ~ expr => FL(name, expr)}

  def Y: Parser[ExprTree] = wholeNumber ^^ {x => Number(x.toInt)} | ident ^^ {x => Value(x.toString)} //todo: | ("(" ~ X ~ ")")

  //  def V: Parser[Any] = ("{" ~> ((A <~ "|") ~ repsep(F, ";")) <~ "}") | repsep(F, ";")

  //  def X: Parser[Any] = repsep(Y, f)

  //  def n: Parser[Any] = ident // todo

  //  def s: Parser[Any] = ident

  //  def f: Parser[Operator] = ("+" | "-" | "*" | "/" | "==" | "<" | ">" | "<>" | "<=" | ">=") ^^ {x => Operator(x.toString)}
}