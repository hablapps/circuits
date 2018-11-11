package annotations

import cats.data._, cats._

trait Annotated[Ann[_[_], _], Alg[_[_]]]{

  def run[P[_]](implicit C: Alg[P]): Ann[P, ?] ~> P

  def inj[P[_]]: P ~> Ann[P, ?]
}

object Annotated{

  object Syntax{

    implicit class RunOp[Ann[_[_], _], P[_], T](p: Ann[P, T]){
      def runP[Alg[_[_]]](implicit C: Alg[P], A: Annotated[Ann, Alg]): P[T] =
        A.run[P](C).apply(p)
    }

    implicit class InjOp[P[_], T](p: P[T]){
      def inj[Ann[_[_], _], Alg[_[_]]](implicit A: Annotated[Ann, Alg]): Ann[P, T] =
        A.inj[P].apply(p)
    }
  }
}
