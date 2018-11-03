package circuits

import cats.Eq

sealed abstract class Xor[P[_], T]

object Xor{
  case class Unk[P[_], T](p: P[T]) extends Xor[P, T]
  case class Or[P[_]](var1: P[Boolean], var2: P[Boolean]) extends Xor[P, Boolean]
  case class And[P[_]](var1: P[Boolean], var2: P[Boolean]) extends Xor[P, Boolean]
  case class Not[P[_]](var1: P[Boolean], var2: P[Boolean]) extends Xor[P, Boolean]

  def run[P[_], T](c: Xor[P, T])(implicit C: Circuit[P]): P[T] = c match {
    case Unk(p) => p
    case Or(var1, var2) => C.or(var1, var2)
    case Not(var1, var2) => C.not(C.and(var1, var2))
    case And(var1, var2) => C.and(var1, var2)
  }

  implicit def XorCircuit[P[_]: Circuit: Forall[Eq, ?[_]]] = new Circuit[Xor[P, ?]]{

    def lit(b: Boolean): Xor[P, Boolean] =
      Unk(Circuit[P].lit(b))

    def and(p1: Xor[P, Boolean], p2: Xor[P, Boolean]): Xor[P, Boolean] =
      (p1, p2) match {
        case (Or(v1, v2), Not(v11, v21))
          if Forall[Eq, P].apply.eqv(v1, v11) &&
             Forall[Eq, P].apply.eqv(v2, v21) =>
          Unk(Circuit[P].xor(v1, v2))
        case (p1, p2) =>
          And(run(p1), run(p2))
      }

    def or(p1: Xor[P, Boolean], p2: Xor[P, Boolean]): Xor[P, Boolean] =
      Or(run(p1), run(p2))

    def not(p: Xor[P, Boolean]): Xor[P, Boolean] =
      p match {
        case And(v1, v2) => Not(v1, v2)
        case p => Unk(run(p))
      }
  }
}
