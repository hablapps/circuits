package circuits
package mtl

import cats.data._
import cats._, cats.implicits._
import annotations._

sealed abstract class IsNeg[T]
case class No[T]() extends IsNeg[T]
case class Yes() extends IsNeg[Boolean]

object IsNeg{

  implicit val AnnotatedIsNeg = new Annotated[IsNeg, Circuit]{

    def run[P[_]](implicit C: Circuit[P]): Annotated.Program[IsNeg, P, ?] ~> P =
      λ[Annotated.Program[IsNeg, P, ?] ~> P]{ c =>
        c.written match {
          case No() => c.value
          case Yes() => C.not(c.value)
        }
      }

    def default[T]: IsNeg[T] = No()
  }

  implicit def AnnotatedCircuit[P[_]: Circuit] = new Circuit.Trivial[IsNeg, P]{
    override def not(p: Annotated.Program[IsNeg, P, Boolean]): Annotated.Program[IsNeg, P, Boolean] =
      if (p.written == Yes()) AnnotatedIsNeg.inj.apply[Boolean](p.value)
      else p.value.writer(Yes())
  }
}
