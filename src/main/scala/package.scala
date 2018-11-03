package circuits


trait Forall[Q[_], P[_]]{
  def apply[T]: Q[P[T]]
}

object Forall{

  def apply[Q[_], P[_]](implicit F: Forall[Q, P]) = F

  import cats.Eq, cats.data.Const

  implicit def EqConstForall[A: Eq] = new Forall[Eq, Const[A, ?]]{
    def apply[T]: Eq[Const[A, T]] = implicitly
  }
}

