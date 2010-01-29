package psic

import scala.util.parsing.combinator._

/*
 * author: ignatov
 */

class PSIParser extends JavaTokenParsers {
  def pack: Parser[Package] = ("P" ~> ident) ~ ("{" ~> repsep(relation, ";") <~ "}") ^^ {
    case name ~ lst => Package(name, lst)
  }

  def relation: Parser[ExprTree] = scheme

  def scheme: Parser[Scheme] = ("S" ~> ident) ~ ("{" ~> rep(expr) <~ "}") ^^ {
    case name ~ lst => Scheme(name, lst)
  }

  def attrdef: Parser[ExprTree] = r ~ (ident <~ ";") ^^ {
    case expr ~ name => AttrDef(name, expr)
  }

  def attrassign: Parser[ExprTree] = (ident <~ "<-") ~ (expr <~ ";") ^^ {
    case name ~ expr => AttrAssign(name, expr)
  }

  def expr: Parser[ExprTree] = attrassign | attrdef

  // primitive type
  def r: Parser[ExprTree] = ("bool" | "nat" | "int" | "string" | "real") ^^ {a => Type(a)}

  def A: Parser[Any] = repsep(((r|s) ~ repsep(ident, ",")), ";") //todo: add

  def P: Parser[Any] = ("P" ~> ident) ~ ("{" ~> repsep(R, ";") <~ "}")

  def R: Parser[Any] = S //todo

  def S: Parser[Any]= ("S" ~> ident) ~ ("{" ~ A ~ "|" ~ repsep(F, ";") ~ "}")

  def F: Parser[Any] = ident ~ "<-" ~ Y // todo: X

  def V: Parser[Any] = ("{" ~ ((A ~ "|") ~ repsep(F, ";")) ~ "}") | repsep(F, ";")

  def X: Parser[Any] = repsep(Y, f)

  def Y: Parser[Any] = (n | wholeNumber)//todo: | ("(" ~ X ~ ")")

  def n: Parser[Any] = ident // todo

  def s: Parser[Any] = ident

  def f: Parser[Any] = ("+" | "-" | "*" | "/" | "==" | "<" | ">" | "<>" | "<=" | ">=")
}