package circuits

case class Par[T](str: String, l: Int)

object Par{

  def run[T](c: Par[T]): String = c match {
    case Par(p, _) => p
  }

  implicit def ParCircuit = new Circuit[Par]{

    def writePar(b: Boolean, str: String): String =
      if (b) s"( $str )" else str

    def lit(b: Boolean): Par[Boolean] =
      Par(if (b) "T" else "F", 0)

    def not(p: Par[Boolean]): Par[Boolean] =
      Par("!" + writePar(p.l > 1, p.str), 1)

    def and(p1: Par[Boolean], p2: Par[Boolean]): Par[Boolean] = {
      val p1str = writePar(p1.l > 2, p1.str)
      val p2str = writePar(p2.l > 2, p2.str)
      Par(s"$p1str && $p2str", 2)
    }

    def or(p1: Par[Boolean], p2: Par[Boolean]): Par[Boolean] =
      Par(s"${p1.str} || ${p2.str}", 3)
  }
}
