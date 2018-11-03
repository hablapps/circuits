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
    }
  }

  describe("Constant propagation interpreter"){

    it("should work with String interpreter"){
      val CPExamples = new Examples[CP[Const[String, ?], ?]](
        Lit(true), Lit(false))
      import CPExamples._

      CP(ex1).getConst shouldBe "T"
      CP(ex2).getConst shouldBe "T"
    }

    it("should work with Eval interpreter"){
      val CPExamples = new Examples[CP[Eval, ?]](
        Lit[Eval](true), Lit[Eval](false))
      import CPExamples._

      CP(ex1).value shouldBe true
      CP(ex2).value shouldBe true
    }
  }
}
