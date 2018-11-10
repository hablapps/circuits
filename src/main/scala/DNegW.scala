package circuits

import cats.evidence.Is

case class DNegW[P[_], T](p: P[T], isNeg: Option[T Is Boolean])

object DNegW{

  def run[P[_], T](c: DNegW[P, T])(implicit C: Circuit[P]): P[T] =
    c.isNeg.fold(c.p){
      isBool => isBool.flip.substitute[P](
        C.not(isBool.substitute[P](c.p))
      )
    }

  implicit def DNegWCircuit[P[_]: Circuit] = new Circuit[DNegW[P, ?]]{

    def lit(b: Boolean): DNegW[P, Boolean] =
      DNegW(Circuit[P].lit(b), None)

    def and(p1: DNegW[P, Boolean], p2: DNegW[P, Boolean]): DNegW[P, Boolean] =
      DNegW(Circuit[P].and(DNegW.run(p1), DNegW.run(p2)), None)

    def or(p1: DNegW[P, Boolean], p2: DNegW[P, Boolean]): DNegW[P, Boolean] =
      DNegW(Circuit[P].or(DNegW.run(p1), DNegW.run(p2)), None)

    def not(p: DNegW[P, Boolean]): DNegW[P, Boolean] =
      if (p.isNeg.isDefined) DNegW(p.p, None)
      else DNegW(DNegW.run(p), Option(Is.refl[Boolean]))
  }
}
