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

  def A: Parser[List[Attribute]] = repsep(attributes, ";") ^^ {lst => lst.foldLeft(List[Attribute]()) {_ ::: _}}

  def P: Parser[Package] = ("P" ~> ident) ~ ("{" ~> repsep((S|Q), ";") <~ "}") ^^ { //todo: uninline R
    case name ~ relations => Package(name, relations)
  }

//  def R: Parser[ExprTree] = (S|Q) ^^ {x => x}

  def S: Parser[Scheme] = ("S" ~> ident) ~ ("{" ~> A) ~ ("|" ~> repsep(F, ";") <~ "}") ^^ {
    case name ~ attributes ~ fls => Scheme(name, attributes, fls)
  }

  def F: Parser[FL] = (ident <~ "<-") ~ Y ^^ {case result ~ expr => FL(result, expr)}

  def Y: Parser[ExprTree] = (wholeNumber ^^ {x => Number(x.toInt)}
          | n
          | "(" ~> X <~ ")")

  def X: Parser[ExprTree] = (Y ~ rep(op ~ Y)) ^^ {
    case a ~ lst => (a /: lst) {
      case (x, op ~ y) => Operator(x, y, op)
    }
  }

  def Q: Parser[Task] = ("Q" ~> ident) ~ ("{" ~ "on" ~> ident <~ "in") ~ (repsep(n, ",")) ~ ("out" ~> repsep(n, ",") <~ "}") ^^ {
    case name ~ scheme ~ in ~ out => Task(name, scheme, in, out)
  }

//  def sQ: Parser[Task] = ("Q" ~> ident) ^^ {
//    case name => Task(name, "sp", Nil, Nil)
//  }

  def G: Parser[ExprTree] = X

  //  def V: Parser[Any] = ("{" ~> ((A <~ "|") ~ repsep(F, ";")) <~ "}") | repsep(F, ";")

  def n: Parser[ExprTree] = ident ~ opt("." ~> ident) ^^ {
    case e ~ None => Expr(e)
    case e ~ Some(sub) => AttrWithSubAttr(e, sub)
  }

  // `f` in PSI-Defs
  def op: Parser[String] = ("+" | "-" | "*" | "/" | "==" | "<" | ">" | "<>" | "<=" | ">=")
}