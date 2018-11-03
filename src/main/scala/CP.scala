package circuits

sealed abstract class CP[P[_], T]
case class Unk[P[_], T](p: P[T]) extends CP[P, T]
case class Lit[P[_]](b: Boolean) extends CP[P, Boolean]

object CP{

  def apply[P[_], T](c: CP[P, T])(implicit C: Circuit[P]): P[T] = c match {
    case Unk(p) => p
    case Lit(b) => C.lit(b)
  }

  implicit def CPCircuit[P[_]: Circuit] = new Circuit[CP[P, ?]]{

    def lit(b: Boolean): CP[P, Boolean] = Lit(b)

    def and(p1: CP[P, Boolean], p2: CP[P, Boolean]): CP[P, Boolean] =
      (p1, p2) match {
        case (Lit(true), p) => p
        case (p, Lit(true)) => p
        case (Lit(false), p) => Lit(false)
        case (p, Lit(false)) => Lit(false)
        case (Unk(p1), Unk(p2)) => Unk(Circuit[P].and(p1, p2))
      }

    def or(p1: CP[P, Boolean], p2: CP[P, Boolean]): CP[P, Boolean] =
      (p1, p2) match {
        case (Lit(true), p) => Lit(true)
        case (p, Lit(true)) => Lit(true)
        case (Lit(false), p) => p
        case (p, Lit(false)) => p
        case (Unk(p1), Unk(p2)) => Unk(Circuit[P].and(p1, p2))
      }

    def not(p: CP[P, Boolean]): CP[P, Boolean] =
      p match {
        case Lit(true) => Lit(false)
        case Lit(false) => Lit(true)
        case Unk(p) => Unk(Circuit[P].not(p))
      }
  }
}
