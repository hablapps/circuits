package circuits
package adhoc

sealed abstract class BNegDown[P[_], T]

object BNegDown{
  case class Unk[P[_], T](p: P[T]) extends BNegDown[P, T]
  case class And[P[_]](p1: P[Boolean], p2: P[Boolean]) extends BNegDown[P, Boolean]
  case class Or[P[_]](p1: P[Boolean], p2: P[Boolean]) extends BNegDown[P, Boolean]

  def run[P[_], T](c: BNegDown[P, T])(implicit C: Circuit[P]): P[T] = c match {
    case Unk(p) => p
    case And(p1, p2) => C.and(p1, p2)
    case Or(p1, p2) => C.or(p1, p2)
  }

  implicit def BNegDownCircuit[P[_]: Circuit] = new Circuit[BNegDown[P, ?]]{

    def lit(b: Boolean): BNegDown[P, Boolean] =
      Unk(Circuit[P].lit(b))

    def and(p1: BNegDown[P, Boolean], p2: BNegDown[P, Boolean]): BNegDown[P, Boolean] =
      And(run(p1), run(p2))

    def or(p1: BNegDown[P, Boolean], p2: BNegDown[P, Boolean]): BNegDown[P, Boolean] =
      Or(run(p1), run(p2))

    def not(p: BNegDown[P, Boolean]): BNegDown[P, Boolean] =
      p match {
        case Unk(p) =>
          Unk(Circuit[P].not(p))
        case And(p1, p2) =>
          Unk(Circuit[P].or(Circuit[P].not(p1), Circuit[P].not(p2)))
        case Or(p1, p2) =>
          Unk(Circuit[P].and(Circuit[P].not(p1), Circuit[P].not(p2)))
      }
  }
}
