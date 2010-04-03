package psi.compiler.parser

import util.parsing.combinator._

/**
 * User: ignatov
 * Date: 04.02.2010
 */

/**
 * Parser for PSI language.
 * PSI Grammar from PSI-Defs.doc
 */
object PSIParser extends JavaTokenParsers {
  /**
   * Primitive type
   */
  def r: Parser[Type] = ("bool" | "nat" | "int" | "string" | "real") ^^ {x => Type(x)}

  /**
   * Sequence of same type attributes
   */
  def attributes: Parser[List[AttributeDef]] =
    (r ~ repsep(ident, ",") ^^ {case t ~ lst => lst.map(AttributeDef(_, t))}) |
      (ident ~ repsep(ident, ",") ^^ {case typename ~ lst => lst.map(AttributeDef(_, Type(typename)))})

  /**
   * Scope with attributes definitions
   */
  def A: Parser[List[AttributeDef]] = repsep(attributes, ";") ^^ {_.foldLeft(List[AttributeDef]()) {_ ::: _}}

  /**
   * Package
   */
  def P: Parser[Package] = ("P" ~> ident) ~ ("{" ~> repsep(rel, ";") <~ "}") ^^ {
    case name ~ relations => Package(name, relations)
  }

  /**
   * Relation: `R` in PSI-Defs
   */
  def rel: Parser[Relation] = Q | S

  /**
   * If statement
   */
  def i: Parser[IfStatement] = ("if" ~> G) ~ ("then" ~> V) ~ ("else" ~> V) <~ "fi" ^^ {
    case condition ~ positive ~ negative => IfStatement(condition, positive, negative)
  }

  /**
   * Scheme
   */
  def S: Parser[Scheme] = ("S" ~> ident) ~ ("{" ~> A) ~ ("|" ~> repsep(F, ";") <~ "}") ~ opt(i) ^^ {
    case name ~ attributes ~ fls ~ None => Scheme(name, null, attributes, fls)
    case name ~ attributes ~ fls ~ Some(condition) => Scheme(name, condition, attributes, fls)
  }

  /**
   * Functional link definition
   */
  def F: Parser[FL] = (n <~ "<-") ~ X ^^ {case result ~ implementation => FL(result, implementation)}

  /**
   * Basic expression: number, attribute occurrence or operator usage
   */
  def Y: Parser[Expression] = (wholeNumber ^^ {x => Number(x.toInt)}
    | n
    | "(" ~> X <~ ")")

  /**
   * Operator usage
   */
  def X: Parser[Expression] = (Y ~ rep(op ~ Y)) ^^ {
    case a ~ lst => (a /: lst) {
      case (x, op ~ y) => Operator(x, y, op)
    }
  }

  /**
   * Task
   */
  def Q: Parser[Task] = ("Q" ~> ident) ~ ("{" ~ "on" ~> ident <~ "in") ~ (repsep(n, ",")) ~ ("out" ~> repsep(n, ",") <~ "}") ^^ {
    case name ~ scheme ~ in ~ out => Task(name, scheme, in, out)
  }

  /**
   * Guard
   */
  def G: Parser[Expression] = X

  /**
   * Variant part
   */
  def V: Parser[Block] = (("{" ~> A) ~ ("|" ~> repsep(F, ";") <~ "}") ^^ {
    case attributes ~ fls => Block(attributes, fls)
  }) | repsep(F, ";") ^^ {Block(Nil, _)}

  /**
   * Attribute occurrence
   */
  def n: Parser[AttributeOccurrence] = ident ~ opt("." ~> ident) ^^ {
    case a ~ None => AttributeOccurrence(a, null)
    case a ~ Some(sub) => AttributeOccurrence(a, sub)
  }

  /**
   * Basic operations: `f` in PSI-Defs
   */
  def op: Parser[String] = ("+" | "-" | "*" | "/" | "==" | "<" | ">" | "<>" | "<=" | ">=")
}