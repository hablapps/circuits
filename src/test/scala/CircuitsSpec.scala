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

  describe("Show interpreter without parenthesis"){

    val P = Circuit[Par]
    import P._
    import Circuit.Syntax._

    it("should not print parenthesis if not needed"){
      Par.run(lit(true)) shouldBe "T"
      Par.run(P.not(P.not(lit(true)))) shouldBe "!!T"
      Par.run(lit(true) and P.not(lit(true)) and lit(true) and lit(false)) shouldBe
        "T && !T && T && F"
      Par.run(lit(true) or P.not(lit(true)) or (lit(true) and lit(false)) or lit(false)) shouldBe
        "T || !T || T && F || F"
    }

    it("should print parentheses if needed"){
      Par.run(P.not(lit(true) and lit(false))) shouldBe
        "!( T && F )"
      Par.run(P.not(lit(true) or lit(false))) shouldBe
        "!( T || F )"
      Par.run(lit(true) and (lit(false) or lit(true))) shouldBe
        "T && ( F || T )"
    }
  }

  describe("Show interpreter without parenthesis (Reader)"){

    val P = Circuit[ParTD]
    import P._
    import Circuit.Syntax._

    it("should not print parenthesis if not needed"){
      ParTD.run(lit(true)) shouldBe "T"
      ParTD.run(P.not(P.not(lit(true)))) shouldBe "!!T"
      ParTD.run(lit(true) and P.not(lit(true)) and lit(true) and lit(false)) shouldBe
        "T && !T && T && F"
      ParTD.run(lit(true) or P.not(lit(true)) or (lit(true) and lit(false)) or lit(false)) shouldBe
        "T || !T || T && F || F"
    }

    it("should print parentheses if needed"){
      ParTD.run(P.not(lit(true) and lit(false))) shouldBe
        "!( T && F )"
      ParTD.run(P.not(lit(true) or lit(false))) shouldBe
        "!( T || F )"
      ParTD.run(lit(true) and (lit(false) or lit(true))) shouldBe
        "T && ( F || T )"
      ParTD.run(
        P.not(lit(true) or P.not(lit(false))) or
        (lit(true) and (lit(true) or lit(false)))) shouldBe
        "!( T || !F ) || T && ( T || F )"
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

  describe("Double negation interpreter (writer)"){

    it("should work with String interpreter"){
      val DNegWExamples = new Examples[DNegW[Const[String, ?], ?]](
        DNegW.DNegWCircuit[Const[String, ?]].lit(true),
        DNegW.DNegWCircuit[Const[String, ?]].lit(false))
      import DNegWExamples._

      DNegW.run(notV1).getConst shouldBe "!T"
      DNegW.run(notnotV1).getConst shouldBe "T"
      DNegW.run(ex1).getConst shouldBe "((T || F) && !(T && F))"
      DNegW.run(ex2).getConst shouldBe "(T && (!F || T))"
    }

    it("should work with Eval interpreter"){
      val DNegWExamples = new Examples[DNegW[Eval, ?]](
        DNegW.DNegWCircuit[Eval].lit(true),
        DNegW.DNegWCircuit[Eval].lit(false))
      import DNegWExamples._

      DNegW.run(ex1).value shouldBe true
      DNegW.run(ex2).value shouldBe true
    }

    it("should work with Constant propagation and Show"){
      val CPExamples = new Examples[CP[DNegW[Const[String, ?], ?], ?]](
        CP.CPCircuit[DNegW[Const[String, ?], ?]].lit(true),
        CP.CPCircuit[DNegW[Const[String, ?], ?]].lit(false))
      import CPExamples._

      DNegW.run(CP.run(notV1)).getConst shouldBe "F"
      DNegW.run(CP.run(notnotV1)).getConst shouldBe "T"
      DNegW.run(CP.run(ex1)).getConst shouldBe "T"
      DNegW.run(CP.run(ex2)).getConst shouldBe "T"
    }

    it("should work with Constant propagation and Eval"){
      val CPExamples = new Examples[CP[DNegW[Eval, ?], ?]](
        CP.CPCircuit[DNegW[Eval, ?]].lit(true),
        CP.CPCircuit[DNegW[Eval, ?]].lit(false))
      import CPExamples._

      DNegW.run(CP.run(notV1)).value shouldBe false
      DNegW.run(CP.run(notnotV1)).value shouldBe true
      DNegW.run(CP.run(ex1)).value shouldBe true
      DNegW.run(CP.run(ex2)).value shouldBe true
    }
  }

  describe("Double negation interpreter (reader)"){

    it("should work with String interpreter"){
      val DNegRExamples = new Examples[DNegR[Const[String, ?], ?]](
        DNegR.DNegRCircuit[Const[String, ?]].lit(true),
        DNegR.DNegRCircuit[Const[String, ?]].lit(false))
      import DNegRExamples._

      DNegR.run(notV1).getConst shouldBe "!T"
      DNegR.run(notnotV1).getConst shouldBe "T"
      DNegR.run(ex1).getConst shouldBe "((T || F) && !(T && F))"
      DNegR.run(ex2).getConst shouldBe "(T && (!F || T))"
    }

    it("should work with Eval interpreter"){
      val DNegRExamples = new Examples[DNegR[Eval, ?]](
        DNegR.DNegRCircuit[Eval].lit(true),
        DNegR.DNegRCircuit[Eval].lit(false))
      import DNegRExamples._

      DNegR.run(ex1).value shouldBe true
      DNegR.run(ex2).value shouldBe true
    }

    it("should work with Constant propagation and Show"){
      val CPExamples = new Examples[CP[DNegR[Const[String, ?], ?], ?]](
        CP.CPCircuit[DNegR[Const[String, ?], ?]].lit(true),
        CP.CPCircuit[DNegR[Const[String, ?], ?]].lit(false))
      import CPExamples._

      DNegR.run(CP.run(notV1)).getConst shouldBe "F"
      DNegR.run(CP.run(notnotV1)).getConst shouldBe "T"
      DNegR.run(CP.run(ex1)).getConst shouldBe "T"
      DNegR.run(CP.run(ex2)).getConst shouldBe "T"
    }

    it("should work with Constant propagation and Eval"){
      val CPExamples = new Examples[CP[DNegR[Eval, ?], ?]](
        CP.CPCircuit[DNegR[Eval, ?]].lit(true),
        CP.CPCircuit[DNegR[Eval, ?]].lit(false))
      import CPExamples._

      DNegR.run(CP.run(notV1)).value shouldBe false
      DNegR.run(CP.run(notnotV1)).value shouldBe true
      DNegR.run(CP.run(ex1)).value shouldBe true
      DNegR.run(CP.run(ex2)).value shouldBe true
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
