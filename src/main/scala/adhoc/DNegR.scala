package circuits

import cats.evidence.Is

case class DNegR[P[_], T](f: Boolean => P[T])

object DNegR{

  def run[P[_], T](c: DNegR[P, T])(implicit C: Circuit[P]): P[T] =
    c.f(false)

  implicit def DNegRCircuit[P[_]: Circuit] = new DNegRCircuitC[P]()

  class DNegRCircuitC[P[_]: Circuit]() extends Circuit[DNegR[P, ?]]{

    def negIf(p: P[Boolean]): Boolean => P[Boolean] =
      if (_) Circuit[P].not(p) else p

    def lit(b: Boolean): DNegR[P, Boolean] =
      DNegR(negIf(Circuit[P].lit(b)))

    def and(p1: DNegR[P, Boolean], p2: DNegR[P, Boolean]): DNegR[P, Boolean] =
      DNegR(negIf(Circuit[P].and(DNegR.run(p1), DNegR.run(p2))))

    def or(p1: DNegR[P, Boolean], p2: DNegR[P, Boolean]): DNegR[P, Boolean] =
      DNegR(negIf(Circuit[P].or(DNegR.run(p1), DNegR.run(p2))))

    def not(p: DNegR[P, Boolean]): DNegR[P, Boolean] =
      DNegR{ if (_) p.f(false) else p.f(true) }
  }
}
