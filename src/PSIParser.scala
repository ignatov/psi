package psic

import scala.util.parsing.combinator._

/*
 * author: ignatov
 */

class PSIParser extends JavaTokenParsers {
  def pack: Parser[Package] = ("P" ~> ident) ~ ("{" ~> rep(relation) <~ "}") ^^ {
    case name ~ lst => Package(name, lst)
  }

  def relation: Parser[ExprTree] = scheme

  def scheme: Parser[Scheme] = ("S" ~> ident) ~ ("{" ~> rep(attrdef) <~ "}") ^^ {
    case name ~ lst => Scheme(name, lst)
  }

  def attrdef: Parser[ExprTree] = r ~ (ident <~ ";") ^^ {
    case expr ~ name => AttrDef(name, expr)
  }

  def attrassign: Parser[ExprTree] = (ident <~ "<-") ~ (expr <~ ";") ^^ {
    case name ~ expr => AttrAssign(name, expr)
  }

  def expr: Parser[ExprTree]

  // primitive type
  def r: Parser[ExprTree] = ("bool" | "nat" | "int" | "string" | "real") ^^ {a => Type(a)}
}