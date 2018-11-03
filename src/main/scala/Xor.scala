package circuits

import cats.data.Const

sealed abstract class Xor[_]

object Xor{
  case class Unk[T](p: Const[String, T]) extends Xor[T]
  case class Or(var1: String, var2: String) extends Xor[Boolean]
  case class And(var1: String, var2: String) extends Xor[Boolean]
  case class Not(var1: String, var2: String) extends Xor[Boolean]


  val ShowC = Circuit[Const[String, ?]]

  def run[T](c: Xor[T]): Const[String, T] = c match {
    case Unk(p) => p
    case Or(var1, var2) => ShowC.or(Const(var1), Const(var2))
    case Not(var1, var2) => ShowC.not(ShowC.and(Const(var1), Const(var2)))
    case And(var1, var2) => ShowC.and(Const(var1), Const(var2))
  }

  implicit def XorCircuit = new Circuit[Xor]{

    def lit(b: Boolean): Xor[Boolean] =
      Unk(ShowC.lit(b))

    def and(p1: Xor[Boolean], p2: Xor[Boolean]): Xor[Boolean] =
      (p1, p2) match {
        case (Or(v1, v2), Not(v11, v21)) if v1 == v11 && v2 == v21 =>
          Unk(ShowC.xor(Const(v1), Const(v2)))
        case (p1, p2) =>
          And(run(p1).getConst, run(p2).getConst)
      }

    def or(p1: Xor[Boolean], p2: Xor[Boolean]): Xor[Boolean] =
      (p1, p2) match {
        case (Unk(v1), Unk(v2)) => Or(v1.getConst, v2.getConst)
        case (p1, p2) => Unk(ShowC.or(run(p1), run(p2)))
      }

    def not(p: Xor[Boolean]): Xor[Boolean] =
      p match {
        case And(v1, v2) => Not(v1, v2)
        case p => Unk(run(p))
      }
  }
}
