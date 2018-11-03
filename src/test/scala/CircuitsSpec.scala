package circuits

import org.scalatest._
import cats.data.Const
import cats.{Eval, Id}

class CircuitsSpec extends FunSpec with Matchers{

  describe("MetaCircular interpreter"){

    val IdExamples = new Examples[Eval](Eval.now(true), Eval.now(false))
    import IdExamples._

    it("should work"){
      ex1.value shouldBe true
    }
  }

  describe("Show interpreter"){

    val ShowExamples = new Examples[Const[String, ?]](
      Const("T"), Const("F"))
    import ShowExamples._

    it("should work"){
      ex1.getConst shouldBe "((T || F) && !(T && F))"
      ex2.getConst shouldBe "(!!T && (!F || T))"
    }
  }

  describe("Constant propagation interpreter"){

    it("should work with String interpreter"){
      val CPExamples = new Examples[CP[Const[String, ?], ?]](
        CP.Lit(true), CP.Lit(false))
      import CPExamples._

      CP.run(ex1).getConst shouldBe "T"
      CP.run(ex2).getConst shouldBe "T"
    }

    it("should work with Eval interpreter"){
      val CPExamples = new Examples[CP[Eval, ?]](
        CP.Lit[Eval](true), CP.Lit[Eval](false))
      import CPExamples._

      CP.run(ex1).value shouldBe true
      CP.run(ex2).value shouldBe true
    }
  }

  describe("Double negation interpreter"){

    it("should work with String interpreter"){
      val DNegExamples = new Examples[DNeg[Const[String, ?], ?]](
        DNeg.DNegCircuit[Const[String, ?]].lit(true),
        DNeg.DNegCircuit[Const[String, ?]].lit(false))
      import DNegExamples._

      DNeg.run(notV1).getConst shouldBe "!T"
      DNeg.run(notnotV1).getConst shouldBe "T"
      DNeg.run(ex1).getConst shouldBe "((T || F) && !(T && F))"
      DNeg.run(ex2).getConst shouldBe "(T && (!F || T))"
    }

    it("should work with Eval interpreter"){
      val DNegExamples = new Examples[DNeg[Eval, ?]](
        DNeg.Unk(Eval.now(true)), DNeg.Unk(Eval.now(false)))
      import DNegExamples._

      DNeg.run(ex1).value shouldBe true
      DNeg.run(ex2).value shouldBe true
    }

    it("should work with Constant propagation and Show"){
      val CPExamples = new Examples[CP[DNeg[Const[String, ?], ?], ?]](
        CP.CPCircuit[DNeg[Const[String, ?], ?]].lit(true),
        CP.CPCircuit[DNeg[Const[String, ?], ?]].lit(false))
      import CPExamples._

      DNeg.run(CP.run(notV1)).getConst shouldBe "F"
      DNeg.run(CP.run(notnotV1)).getConst shouldBe "T"
      DNeg.run(CP.run(ex1)).getConst shouldBe "T"
      DNeg.run(CP.run(ex2)).getConst shouldBe "T"
    }

    it("should work with Constant propagation and Eval"){
      val CPExamples = new Examples[CP[DNeg[Eval, ?], ?]](
        CP.CPCircuit[DNeg[Eval, ?]].lit(true),
        CP.CPCircuit[DNeg[Eval, ?]].lit(false))
      import CPExamples._

      DNeg.run(CP.run(notV1)).value shouldBe false
      DNeg.run(CP.run(notnotV1)).value shouldBe true
      DNeg.run(CP.run(ex1)).value shouldBe true
      DNeg.run(CP.run(ex2)).value shouldBe true
    }
  }

  describe("Xor interpreter"){
    import cats.implicits._

    val XorExamples = new Examples[Xor[Const[String,?],?]](
      Circuit[Xor[Const[String,?], ?]].lit(true),
      Circuit[Xor[Const[String,?], ?]].lit(false))
    import XorExamples._

    it("should work"){
      Xor.run(ex1).getConst shouldBe "(T xor F)"
      Xor.run(xor2).getConst shouldBe "((T xor F) xor F)"
    }
  }

  describe("BNegDown interpreter"){
    import cats.implicits._

    val BNegDownExamples = new Examples[BNegDown[Const[String,?],?]](
      Circuit[BNegDown[Const[String,?], ?]].lit(true),
      Circuit[BNegDown[Const[String,?], ?]].lit(false))

    import BNegDownExamples._

    it("should work"){
      BNegDown.run(ex3).getConst shouldBe "(!(!T && !F) || !T)"
    }
  }
}
