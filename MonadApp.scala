// package monapp

// import cats._, cats.implicits._, cats.data._
// import cats.mtl._, cats.mtl.syntax.all._

// object Test{

//   def foo[P[_]: Applicative: FunctorRaise[?[_], String]]: P[Boolean] =
//     ("error1".raise[P, Boolean], "error2".raise[P, Boolean]).mapN((_,_) => false)

//   def bar[P[_]: Monad: Applicative: FunctorRaise[?[_], String]](): P[Boolean] =
//     1.pure[P] >>= (_ => foo[P])

//   type StateValidated[S, T] = StateT[ValidatedNel[String, ?], S, T]
// Monad[StateT[Either[String, ?], String, ?]]
// Monad[StateT[ValidatedNel[String, ?], String, ?]]
//   // bar[StateValidated[String, ?]]

// }
