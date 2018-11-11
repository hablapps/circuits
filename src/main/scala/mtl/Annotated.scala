package circuits
package mtl

import cats.data._, cats._

trait Annotated[Annotation[_], Alg[_[_]], P[_]]{

  type AnnotatedProgram[P[_], T] = Writer[Annotation[T], P[T]]

  def run[P[_]](implicit C: Alg[P]): AnnotatedProgram[P, ?] ~> P

  def inj[P[_]](implicit D: Annotated.Default[Annotation]): P ~> AnnotatedProgram[P, ?] =
    new (P ~> AnnotatedProgram[P, ?]){
      def apply[T](p: P[T]) = WriterT[Id, Annotation[T], P[T]]((D.apply[T], p))
    }
}

object Annotated{

  trait Default[Annotation[_]]{
    def apply[T]: Annotation[T]
  }
}
