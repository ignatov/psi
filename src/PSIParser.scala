package psic

import util.parsing.combinator._

/*
 * author: ignatov
 */

class PSIParser extends JavaTokenParsers {
  def r: Parser[Type] = ("bool" | "nat" | "int" | "string" | "real") ^^ {a => Type(a)}

  def A: Parser[Any] = repsep(((r | ident) ~ repsep(ident, ",")), ";")

  def P: Parser[Any] = ("P" ~> ident) ~ ("{" ~> repsep(R, ";") <~ "}")

  def R: Parser[Any] = S //todo

  def S: Parser[Any] = ("S" ~> ident) ~ ("{" ~> A) ~ ("|" ~> repsep(F, ";") <~ "}")

  def F: Parser[FL] = (ident <~ "<-") ~ Y ^^ {case name ~ expr => FL(name, expr)}

  def Y: Parser[ExprTree] = wholeNumber ^^ {x => Number(x.toInt)} | ident ^^ {x => Value(x.toString)} //todo: | ("(" ~ X ~ ")")

  //  def V: Parser[Any] = ("{" ~> ((A <~ "|") ~ repsep(F, ";")) <~ "}") | repsep(F, ";")

  //  def X: Parser[Any] = repsep(Y, f)

  //  def n: Parser[Any] = ident // todo

  //  def s: Parser[Any] = ident

  //  def f: Parser[Operator] = ("+" | "-" | "*" | "/" | "==" | "<" | ">" | "<>" | "<=" | ">=") ^^ {x => Operator(x.toString)}
}