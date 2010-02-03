package pcis.metamodel

import collection.mutable.HashMap

/*
 * author: ignatov
 */

case class P(name: String, features: HashMap[String, R])

trait R {def name: String}

case class S(name: String, condition: G, thenBranch: V, elseBranch: V, fls: List[F], features: HashMap[String, A]) extends R

case class Q(name: String, in: List[N], out: List[N]) extends R

case class G(expr: X)

case class A(name: String, t: Type)

case class N(t: Type, args: List[F], ress: List[F], attrs: List[A])

case class V(scheme: S, fls: List[F], features: HashMap[String, A])

case class F(expr: X, res: N)

case class X(impl: String, args: List[N])

case class Type(name: String)
