package circuits

trait Circuit[P[_]]{
  def lit(b: Boolean): P[Boolean]
  def and(p1: P[Boolean], p2: P[Boolean]): P[Boolean]
  def or(p1: P[Boolean], p2: P[Boolean]): P[Boolean]
  def not(p: P[Boolean]): P[Boolean]

  def xor(p1: P[Boolean], p2: P[Boolean]): P[Boolean] =
    and(or(p1, p2), not(and(p1, p2)))
}

object Circuit{

  def apply[P[_]](implicit C: Circuit[P]) = C

  object Syntax{
    implicit class BinOps[P[_]](p1: P[Boolean])(implicit C: Circuit[P]){
      def and(p2: P[Boolean]): P[Boolean] = C.and(p1, p2)
      def or(p2: P[Boolean]): P[Boolean] = C.or(p1, p2)
      def xor(p2: P[Boolean]): P[Boolean] = C.xor(p1, p2)
    }

    def not[P[_]](p: P[Boolean])(implicit C: Circuit[P]): P[Boolean] = C.not(p)
  }

  import cats.Eval, cats.syntax._, cats.implicits._

  implicit val MetaCircular = new Circuit[Eval]{
    def lit(b: Boolean): Eval[Boolean] = Eval.now(b)
    def and(p1: Eval[Boolean], p2: Eval[Boolean]): Eval[Boolean] =
      (p1, p2).mapN(_ && _)
    def or(p1: Eval[Boolean], p2: Eval[Boolean]): Eval[Boolean] =
      (p1, p2).mapN(_ || _)
    def not(p: Eval[Boolean]): Eval[Boolean] =
      p.map(! _)
  }

  import cats.data.Const

  implicit val ShowCircuit = new Circuit[Const[String, ?]]{
    def lit(b: Boolean): Const[String, Boolean] = Const(if (b) "T" else "F")
    def and(p1: Const[String, Boolean],
      p2: Const[String, Boolean]): Const[String, Boolean] =
      Const(s"(${p1.getConst} && ${p2.getConst})")
    def or(p1: Const[String, Boolean],
      p2: Const[String, Boolean]): Const[String, Boolean] =
      Const(s"(${p1.getConst} || ${p2.getConst})")
    def not(p: Const[String, Boolean]): Const[String, Boolean] =
      Const(s"!${p.getConst}")
  }
}
