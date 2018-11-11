package annotations

import cats.data._, cats._

trait Annotated[Ann[_], Alg[_[_]]]{

  def run[P[_]](implicit C: Alg[P]): Annotated.Program[Ann, P, ?] ~> P

  def default[T]: Ann[T]

  def inj[P[_]]: P ~> Annotated.Program[Ann, P, ?] =
    new (P ~> Annotated.Program[Ann, P, ?]){
      def apply[T](p: P[T]) = WriterT[Id, Ann[T], P[T]]((default[T], p))
    }
}

object Annotated{
  type Program[Ann[_], P[_], T] = Writer[Ann[T], P[T]]

  object Syntax{

    implicit class RunOp[Ann[_], P[_], T](p: Program[Ann, P, T]){
      def runP[Alg[_[_]]](implicit C: Alg[P], A: Annotated[Ann, Alg]): P[T] =
        A.run[P](C).apply(p)
    }

    implicit class InjOp[P[_], T](p: P[T]){
      def inj[Ann[_], Alg[_[_]]](implicit A: Annotated[Ann, Alg]): Annotated.Program[Ann, P, T] =
        A.inj[P].apply(p)
    }
  }
}
