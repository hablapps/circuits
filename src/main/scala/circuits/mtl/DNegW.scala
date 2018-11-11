package circuits
package mtl

import cats.data._
import cats._, cats.implicits._
import annotations._

sealed abstract class IsNeg[P[_], T]
case class No[P[_], T](p: P[T]) extends IsNeg[P, T]
case class Yes[P[_]](p: P[Boolean]) extends IsNeg[P, Boolean]

object IsNeg{

  implicit val AnnotatedIsNeg = new Annotated[IsNeg, Circuit]{

    def run[P[_]](implicit C: Circuit[P]): IsNeg[P, ?] ~> P =
      λ[IsNeg[P, ?] ~> P]{
        case No(p) => p
        case Yes(p) => C.not(p)
      }

    def inj[P[_]] =
      λ[P ~> IsNeg[P, ?]]{ No(_) }
  }

  implicit def AnnotatedCircuit[P[_]: Circuit] = new Circuit.Trivial[IsNeg, P]{
    override def not(p: IsNeg[P, Boolean]): IsNeg[P, Boolean] =
      p match {
        case Yes(p) => AnnotatedIsNeg.inj.apply(p)
        case No(p) => Yes(p)
      }
  }
}
