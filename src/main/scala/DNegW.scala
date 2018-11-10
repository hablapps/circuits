package circuits

import cats.evidence.Is

case class DNegW[P[_], T](p: P[T], isNeg: DNegW.IsNeg[T])

object DNegW{

  sealed abstract class IsNeg[T]
  case class No[T]() extends IsNeg[T]
  case class Yes() extends IsNeg[Boolean]

  def run[P[_], T](c: DNegW[P, T])(implicit C: Circuit[P]): P[T] =
    c.isNeg match {
      case No() => c.p
      case Yes() => C.not(c.p)
    }

  implicit def DNegWCircuit[P[_]: Circuit] = new Circuit[DNegW[P, ?]]{

    def lit(b: Boolean): DNegW[P, Boolean] =
      DNegW(Circuit[P].lit(b), No[Boolean]())

    def and(p1: DNegW[P, Boolean], p2: DNegW[P, Boolean]): DNegW[P, Boolean] =
      DNegW(Circuit[P].and(DNegW.run(p1), DNegW.run(p2)), No[Boolean]())

    def or(p1: DNegW[P, Boolean], p2: DNegW[P, Boolean]): DNegW[P, Boolean] =
      DNegW(Circuit[P].or(DNegW.run(p1), DNegW.run(p2)), No[Boolean]())

    def not(p: DNegW[P, Boolean]): DNegW[P, Boolean] =
      if (p.isNeg == Yes()) DNegW(p.p, No[Boolean]())
      else DNegW(DNegW.run(p), Yes())
  }
}
