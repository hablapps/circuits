package circuits
package mtl

sealed abstract class BNegDown[P[_], T]
case class Unk[P[_], T](p: P[T]) extends BNegDown[P, T]
case class And[P[_]](p1: P[Boolean], p2: P[Boolean]) extends BNegDown[P, Boolean]
case class Or[P[_]](p1: P[Boolean], p2: P[Boolean]) extends BNegDown[P, Boolean]

object BNegDown{
  import annotations._
  import cats._

  implicit val Ann = new Annotated[BNegDown, Circuit]{
    def run[P[_]](implicit C: Circuit[P]): BNegDown[P, ?] ~> P =
      λ[BNegDown[P, ?] ~> P]{
        case Unk(p) => p
        case And(p1, p2) => C.and(p1, p2)
        case Or(p1, p2) => C.or(p1, p2)
      }

    def inj[P[_]] = λ[P ~> BNegDown[P, ?]]{ Unk(_) }
  }

  implicit def AnnotatedCircuit[P[_]: Circuit] = new Circuit.Trivial[BNegDown, P]{

    override def and(p1: BNegDown[P, Boolean], p2: BNegDown[P, Boolean]): BNegDown[P, Boolean] =
      And(Ann.run.apply(p1), Ann.run.apply(p2))

    override def or(p1: BNegDown[P, Boolean], p2: BNegDown[P, Boolean]): BNegDown[P, Boolean] =
      Or(Ann.run.apply(p1), Ann.run.apply(p2))

    override def not(p: BNegDown[P, Boolean]): BNegDown[P, Boolean] =
      p match {
        case Unk(p) =>
          Ann.inj(Circuit[P].not(p))
        case And(p1, p2) =>
          Ann.inj(Circuit[P].or(Circuit[P].not(p1), Circuit[P].not(p2)))
        case Or(p1, p2) =>
          Ann.inj(Circuit[P].and(Circuit[P].not(p1), Circuit[P].not(p2)))
    }
  }
}
