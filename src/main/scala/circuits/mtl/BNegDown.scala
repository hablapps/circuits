// package circuits
// package adhoc

// sealed abstract class BNegDown[T]
// case class Unk[T]() extends BNegDown[T]
// case class And[P[_]](p1: P[Boolean], p2: P[Boolean]) extends BNegDown[Boolean]
// case class Or[P[_]](p1: P[Boolean], p2: P[Boolean]) extends BNegDown[Boolean]

// object BNegDown{
//   import annotations._
//   import cats._

//   implicit val AnnotatedBNegDown = new Annotated[BNegDown, Circuit]{
//     def run[P[_], T](implicit C: Circuit[P]): Annotated.Program[BNegDown, P, ?] ~> P =
//       λ[Annotated.Program[BNegDown, P, ?] ~> P]{ c =>
//         c.written match {
//           case Unk() => c.value
//           case And(p1, p2) => C.and(p1, p2)
//           case Or(p1, p2) => C.or(p1, p2)
//         }
//       }

//     def default[T] = Unk[T]()
//   }

//   // implicit def BNegDownCircuit[P[_]: Circuit] = new Circuit[BNegDown[P, ?]]{

//   //   def lit(b: Boolean): BNegDown[P, Boolean] =
//   //     Unk(Circuit[P].lit(b))

//   //   def and(p1: BNegDown[P, Boolean], p2: BNegDown[P, Boolean]): BNegDown[P, Boolean] =
//   //     And(run(p1), run(p2))

//   //   def or(p1: BNegDown[P, Boolean], p2: BNegDown[P, Boolean]): BNegDown[P, Boolean] =
//   //     Or(run(p1), run(p2))

//   //   def not(p: BNegDown[P, Boolean]): BNegDown[P, Boolean] =
//   //     p match {
//   //       case Unk(p) =>
//   //         Unk(Circuit[P].not(p))
//   //       case And(p1, p2) =>
//   //         Unk(Circuit[P].or(Circuit[P].not(p1), Circuit[P].not(p2)))
//   //       case Or(p1, p2) =>
//   //         Unk(Circuit[P].and(Circuit[P].not(p1), Circuit[P].not(p2)))
//   //     }
//   // }
// }
