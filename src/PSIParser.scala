package psic

import scala.util.parsing.combinator._

/*
 * author: ignatov
 */

class PSIParser extends JavaTokenParsers {
  def pack: Parser[Package] = ("P" ~> ident) ~ ("{" ~> rep(scheme) <~ "}") ^^ {
    case name ~ lst => Package(name, lst)
  }

  def scheme: Parser[Scheme] = ("S" ~> ident) ~ ("{" ~> rep(attrdef) <~ "}") ^^ {
    case name ~ lst => Scheme(name, lst)
  }

  def attrdef: Parser[ExprTree] = r ~ (ident <~ ";") ^^ {
    case expr ~ name => AttrDef(name, expr)
  }

  // primitive type
  def r: Parser[ExprTree] = ("bool" | "nat" | "int" | "string" | "real") ^^ {a => Type(a)}
}