package psic.parser

import util.parsing.combinator._

/**
 * @author: ignatov
 * Date:    04.02.2010
 */

class PSIParser extends JavaTokenParsers {
  def r: Parser[Type] = ("bool" | "nat" | "int" | "string" | "real") ^^ {x => Type(x)}

  def attributes: Parser[List[AttributeDef]] = (r ~ repsep(ident, ",") ^^ {
    case t ~ lst => lst.map(AttributeDef(_, t))
  }) | (ident ~ repsep(ident, ",") ^^ {
    case typename ~ lst => lst.map(AttributeDef(_, Type(typename)))
  })

  def A: Parser[List[AttributeDef]] = repsep(attributes, ";") ^^ {_.foldLeft(List[AttributeDef]()) {_ ::: _}}

  def P: Parser[Package] = ("P" ~> ident) ~ ("{" ~> repsep(rel, ";") <~ "}") ^^ {
    case name ~ relations => Package(name, relations)
  }

  // `R` in PSI-Defs
  def rel: Parser[Relation] = Q | S

  def i: Parser[IfExpr] = ("if" ~> G) ~ ("then" ~> V) ~ ("else" ~> V) <~ "fi" ^^ {
    case condition ~ positive ~ negative => IfExpr(condition, positive, negative)
  }

  def S: Parser[Scheme] = ("S" ~> ident) ~ ("{" ~> A) ~ ("|" ~> repsep(F, ";") <~ "}") ~ opt(i) ^^ {
    case name ~ attributes ~ fls ~ None => Scheme(name, null, attributes, fls)
    case name ~ attributes ~ fls ~ Some(condition) => Scheme(name, condition, attributes, fls)
  }

  def F: Parser[FL] = (n <~ "<-") ~ X ^^ {case result ~ implementation => FL(result, implementation)}

  def Y: Parser[Expression] = (wholeNumber ^^ {x => Number(x.toInt)}
    | n
    | "(" ~> X <~ ")")

  def X: Parser[Expression] = (Y ~ rep(op ~ Y)) ^^ {
    case a ~ lst => (a /: lst) {
      case (x, op ~ y) => Operator(x, y, op)
    }
  }

  def Q: Parser[Task] = ("Q" ~> ident) ~ ("{" ~ "on" ~> ident <~ "in") ~ (repsep(n, ",")) ~ ("out" ~> repsep(n, ",") <~ "}") ^^ {
    case name ~ scheme ~ in ~ out => Task(name, scheme, in, out)
  }

  def G: Parser[Expression] = X

  def V: Parser[Block] = (("{" ~> A) ~ ("|" ~> repsep(F, ";") <~ "}") ^^ {
    case attributes ~ fls => Block(attributes, fls)
  }) | repsep(F, ";") ^^ {Block(Nil, _)}

  def n: Parser[AttributeOccurrence] = ident ~ opt("." ~> ident) ^^ {
    case a ~ None => AttributeOccurrence(a, null)
    case a ~ Some(sub) => AttributeOccurrence(a, sub)
  }

  // `f` in PSI-Defs
  def op: Parser[String] = ("+" | "-" | "*" | "/" | "==" | "<" | ">" | "<>" | "<=" | ">=")
}