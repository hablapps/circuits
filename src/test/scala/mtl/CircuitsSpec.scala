package circuits
package mtl

import org.scalatest._
import cats.data.Const
import cats.{Eval, Id}

class CircuitsSpec extends FunSpec with Matchers{

  describe("Double negation interpreter (WriterT)"){
    type AnnPrg[P[_], T] = annotations.Annotated.Program[IsNeg, P, T]
    import annotations.Annotated.Syntax._

    it("should work with String interpreter"){
      val AnnPrgExamples = new Examples[AnnPrg[Const[String, ?], ?]](
        IsNeg.AnnotatedCircuit[Const[String, ?]].lit(true),
        IsNeg.AnnotatedCircuit[Const[String, ?]].lit(false))
      import AnnPrgExamples._

      notV1.runP[Circuit].getConst shouldBe "!T"
      notV1.runP[Circuit].getConst shouldBe "!T"
      notnotV1.runP[Circuit].getConst shouldBe "T"
      ex1.runP[Circuit].getConst shouldBe "((T || F) && !(T && F))"
      ex2.runP[Circuit].getConst shouldBe "(T && (!F || T))"
    }

    it("should work with Eval interpreter"){
      val AnnPrgExamples = new Examples[AnnPrg[Eval, ?]](
        IsNeg.AnnotatedCircuit[Eval].lit(true),
        IsNeg.AnnotatedCircuit[Eval].lit(false))
      import AnnPrgExamples._

      ex1.runP[Circuit].value shouldBe true
      ex2.runP[Circuit].value shouldBe true
    }
  }

  // describe("Double negation interpreter (reader)"){

  //   it("should work with String interpreter"){
  //     val DNegRExamples = new Examples[DNegR[Const[String, ?], ?]](
  //       DNegR.DNegRCircuit[Const[String, ?]].lit(true),
  //       DNegR.DNegRCircuit[Const[String, ?]].lit(false))
  //     import DNegRExamples._

  //     DNegR.run.apply(notV1).getConst shouldBe "!T"
  //     DNegR.run.apply(notnotV1).getConst shouldBe "T"
  //     DNegR.run.apply(ex1).getConst shouldBe "((T || F) && !(T && F))"
  //     DNegR.run.apply(ex2).getConst shouldBe "(T && (!F || T))"
  //   }

  //   it("should work with Eval interpreter"){
  //     val DNegRExamples = new Examples[DNegR[Eval, ?]](
  //       DNegR.DNegRCircuit[Eval].lit(true),
  //       DNegR.DNegRCircuit[Eval].lit(false))
  //     import DNegRExamples._

  //     DNegR.run.apply(ex1).value shouldBe true
  //     DNegR.run.apply(ex2).value shouldBe true
  //   }

  //   it("should work with Constant propagation and Show"){
  //     val CPExamples = new Examples[CP[DNegR[Const[String, ?], ?], ?]](
  //       CP.CPCircuit[DNegR[Const[String, ?], ?]].lit(true),
  //       CP.CPCircuit[DNegR[Const[String, ?], ?]].lit(false))
  //     import CPExamples._

  //     DNegR.run.apply(CP.run.apply(notV1)).getConst shouldBe "F"
  //     DNegR.run.apply(CP.run.apply(notnotV1)).getConst shouldBe "T"
  //     DNegR.run.apply(CP.run.apply(ex1)).getConst shouldBe "T"
  //     DNegR.run.apply(CP.run.apply(ex2)).getConst shouldBe "T"
  //   }

  //   it("should work with Constant propagation and Eval"){
  //     val CPExamples = new Examples[CP[DNegR[Eval, ?], ?]](
  //       CP.CPCircuit[DNegR[Eval, ?]].lit(true),
  //       CP.CPCircuit[DNegR[Eval, ?]].lit(false))
  //     import CPExamples._

  //     DNegR.run.apply(CP.run.apply(notV1)).value shouldBe false
  //     DNegR.run.apply(CP.run.apply(notnotV1)).value shouldBe true
  //     DNegR.run.apply(CP.run.apply(ex1)).value shouldBe true
  //     DNegR.run.apply(CP.run.apply(ex2)).value shouldBe true
  //   }
  // }

  // describe("Xor interpreter"){
  //   import cats.implicits._

  //   val XorExamples = new Examples[Xor[Const[String,?],?]](
  //     Circuit[Xor[Const[String,?], ?]].lit(true),
  //     Circuit[Xor[Const[String,?], ?]].lit(false))
  //   import XorExamples._

  //   it("should work"){
  //     Xor.run.apply(ex1).getConst shouldBe "(T xor F)"
  //     Xor.run.apply(xor2).getConst shouldBe "((T xor F) xor F)"
  //   }
  // }

  // describe("BNegDown interpreter"){
  //   import cats.implicits._

  //   val BNegDownExamples = new Examples[BNegDown[Const[String,?],?]](
  //     Circuit[BNegDown[Const[String,?], ?]].lit(true),
  //     Circuit[BNegDown[Const[String,?], ?]].lit(false))

  //   import BNegDownExamples._

  //   it("should work"){
  //     BNegDown.run.apply(ex3).getConst shouldBe "(!(!T && !F) || !T)"
  //   }
  // }
}
