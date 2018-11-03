package circuits

sealed abstract class DNeg[P[_], T]

object DNeg{
  case class Unk[P[_], T](p: P[T]) extends DNeg[P, T]
  case class Neg[P[_]](p: P[Boolean]) extends DNeg[P, Boolean]

  def run[P[_], T](c: DNeg[P, T])(implicit C: Circuit[P]): P[T] = c match {
    case Unk(p) => p
    case Neg(p) => C.not(p)
  }

  implicit def DNegCircuit[P[_]: Circuit] = new Circuit[DNeg[P, ?]]{

    def lit(b: Boolean): DNeg[P, Boolean] =
      Unk(Circuit[P].lit(b))

    def and(p1: DNeg[P, Boolean], p2: DNeg[P, Boolean]): DNeg[P, Boolean] =
      Unk(Circuit[P].and(DNeg.run(p1), DNeg.run(p2)))

    def or(p1: DNeg[P, Boolean], p2: DNeg[P, Boolean]): DNeg[P, Boolean] =
      Unk(Circuit[P].or(DNeg.run(p1), DNeg.run(p2)))

    def not(p: DNeg[P, Boolean]): DNeg[P, Boolean] =
      p match {
        case Neg(p) => Unk(p)
        case Unk(p) => Neg(p)
      }
  }
}
