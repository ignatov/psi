package pcis.metamodel

import collection.mutable.HashMap

/**
 * @author: ignatov
 * Date:    04.02.2010
 */

/**
 * Package
 */
case class P(name: String, features: HashMap[String, R])

/**
 * Relation
 */
trait R {def name: String}

/**
 * Scheme
 */
case class S(name: String, condition: G, thenBranch: V, elseBranch: V, fls: List[F], features: HashMap[String, A]) extends R

/**
 * Task
 */
case class Q(name: String, in: List[N], out: List[N]) extends R

/**
 * Guard
 */
case class G(expr: X)

/**
 * Attribute
 */
case class A(name: String, t: T)

/**
 * Attribute occuraNce
 */
case class N(t: T, args: List[F], ress: List[F], attrs: List[A])

/**
 * Variant part
 */
case class V(scheme: S, fls: List[F], features: HashMap[String, A])

/**
 * Functional link
 */
case class F(expr: X, res: N)

/**
 * eXpression
 */
case class X(impl: String, args: List[N])

/**
 * Type
 */
case class T(name: String)
