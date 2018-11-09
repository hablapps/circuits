package circuits

case class ParTD[T](f: Int => String)

object ParTD{

  def run[T](c: ParTD[T]): String =
    c.f(3)

  implicit def ParTDCircuit = new Circuit[ParTD]{

    def writeParTD(b: Boolean, str: String): String =
      if (b) s"( $str )" else str

    def lit(b: Boolean): ParTD[Boolean] =
      ParTD(_ => (if (b) "T" else "F"))

    def not(p: ParTD[Boolean]): ParTD[Boolean] =
      ParTD(_ => "!" + p.f(1))

    def and(p1: ParTD[Boolean], p2: ParTD[Boolean]): ParTD[Boolean] = {
      val p1str = p1.f(2)
      val p2str = p2.f(2)
      ParTD(l => writeParTD(l < 2, s"$p1str && $p2str"))
    }

    def or(p1: ParTD[Boolean], p2: ParTD[Boolean]): ParTD[Boolean] =
      ParTD(l => writeParTD(l < 3, s"${p1.f(3)} || ${p2.f(3)}"))
  }
}
