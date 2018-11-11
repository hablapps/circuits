package circuits
package mtl

import cats.data._
import cats._, cats.implicits._

sealed abstract class IsNeg[T]
case class No[T]() extends IsNeg[T]
case class Yes() extends IsNeg[Boolean]

object IsNeg{

  type DNegW[P[_], T] = Writer[IsNeg[T], P[T]]

  def run[P[_]](implicit C: Circuit[P]): DNegW[P, ?] ~> P =
    λ[DNegW[P, ?] ~> P]{ c =>
      c.written match {
        case No() => c.value
        case Yes() => C.not(c.value)
      }
    }

  def inj[P[_]]: P ~> DNegW[P, ?] =
    new (P ~> DNegW[P, ?]){
      def apply[T](p: P[T]) = WriterT[Id, IsNeg[T], P[T]]((No(), p))
    }

  implicit def DNegWCircuit[P[_]: Circuit] = new Circuit[DNegW[P, ?]]{

    def lit(b: Boolean): DNegW[P, Boolean] =
      inj(Circuit[P].lit(b))

    def and(p1: DNegW[P, Boolean], p2: DNegW[P, Boolean]): DNegW[P, Boolean] =
      inj(Circuit[P].and(run.apply(p1), run.apply(p2)))

    def or(p1: DNegW[P, Boolean], p2: DNegW[P, Boolean]): DNegW[P, Boolean] =
      inj(Circuit[P].or(run.apply(p1), run.apply(p2)))

    def not(p: DNegW[P, Boolean]): DNegW[P, Boolean] =
      if (p.written == Yes()) inj.apply[Boolean](p.value)
      else p.value.writer(Yes())
  }
}
