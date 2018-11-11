package circuits
package mtl

import org.scalatest._
import cats.data.Const
import cats.{Eval, Id}
import annotations.Annotated.Syntax._

class CircuitsSpec extends FunSpec with Matchers{

  describe("Double negation interpreter (WriterT)"){

    it("should work with String interpreter"){
      val IsNegExamples = new Examples[IsNeg[Const[String, ?], ?]](
        IsNeg.AnnotatedCircuit[Const[String, ?]].lit(true),
        IsNeg.AnnotatedCircuit[Const[String, ?]].lit(false))
      import IsNegExamples._

      notV1.runP[Circuit].getConst shouldBe "!T"
      notV1.runP[Circuit].getConst shouldBe "!T"
      notnotV1.runP[Circuit].getConst shouldBe "T"
      ex1.runP[Circuit].getConst shouldBe "((T || F) && !(T && F))"
      ex2.runP[Circuit].getConst shouldBe "(T && (!F || T))"
    }

    it("should work with Eval interpreter"){
      val IsNegExamples = new Examples[IsNeg[Eval, ?]](
        IsNeg.AnnotatedCircuit[Eval].lit(true),
        IsNeg.AnnotatedCircuit[Eval].lit(false))
      import IsNegExamples._

      ex1.runP[Circuit].value shouldBe true
      ex2.runP[Circuit].value shouldBe true
    }
  }


  describe("BNegDown interpreter"){
    import cats.implicits._

    val BNegDownExamples = new Examples[BNegDown[Const[String,?],?]](
      Circuit[BNegDown[Const[String,?], ?]].lit(true),
      Circuit[BNegDown[Const[String,?], ?]].lit(false))

    import BNegDownExamples._

    it("should work"){
      ex3.runP[Circuit].getConst shouldBe "(!(!T && !F) || !T)"
    }
  }
}
