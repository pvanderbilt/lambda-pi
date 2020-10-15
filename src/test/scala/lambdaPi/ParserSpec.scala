package lambdaPi.test

import org.scalatest._
import flatspec._
import matchers._

import lambdaPi.syntax._
import lambdaPi.parser._

class ParserSpec extends AnyFlatSpec with should.Matchers {

  val parser = lambdaPiParser

  "The lambda-pi parser" should "exist" in { assert(parser!=null) }

  def parseChecked (in: String): Term =
    parser.parseTermRaw(in) match {
      case parser.Success(ast, _) => ast
      case parser.NoSuccess(msg, rem) =>
        fail(s"Parsing of '${in}' yielded 'NoSuccess'; \n\tmsg = ${msg}; \n\tremaining: ${rem.source}")
      case parser.Error(msg, _) => fail(s"Parsing of '${in}' yielded 'Error'; msg = ${msg}")
      case parser.Failure(msg, _) => fail(s"Parsing of '${in}' yielded 'Failure'; msg = ${msg}")
    }

  def checkParse (in: String, expectedAST: Term = null): Unit = {
    val actAST = parseChecked(in)
    if (expectedAST == null) {
      info(s"""parse "${in}" parsed as ${actAST}""");
      succeed
    }
    it should s"""parse "${in}" to ${expectedAST}""" in {
      assert(actAST == expectedAST)
    }
  }

  checkParse("x", FVar(NGlobal("x")))
  checkParse("#1", BVar(1))
  checkParse("*", Type())
  checkParse(" * ", Type())
  checkParse("λ#1", Lam(BVar(1)))
  checkParse("λλ#1", Lam(Lam(BVar(1))))
  checkParse("f x", App(FVar(NGlobal("f")), FVar(NGlobal("x"))))
  checkParse("f y z", App(App(FVar(NGlobal("f")),FVar(NGlobal("y"))),FVar(NGlobal("z"))))
  checkParse("x=>y", Pi(FVar(NGlobal("x")),FVar(NGlobal("y"))))
  checkParse("x=>*=>*", Pi(FVar(NGlobal("x")), Pi(Type(),Type())))
  checkParse("x=>y=>z", Pi(FVar(NGlobal("x")),Pi(FVar(NGlobal("y")),FVar(NGlobal("z")))))
  checkParse("(*=>*)=>*", Pi(Pi(Type(),Type()), Type()))
  checkParse("*=>(*=>*)", Pi(Type(), Pi(Type(),Type())))
  checkParse("x:*", Ann(FVar(NGlobal("x")),Type()))
  checkParse("f x : * => *", Ann(App(FVar(NGlobal("f")),FVar(NGlobal("x"))),Pi(Type(),Type())))
  checkParse("x=>#0=>#1", Pi(FVar(NGlobal("x")),Pi(BVar(0),BVar(1))))
  checkParse("(λλ #1) x y", App(App( Lam(Lam(BVar(1))), FVar(NGlobal("x"))), FVar(NGlobal("y"))))
  checkParse("(*:*)", Ann(Type(),Type()))
  checkParse("λλλ#1 : *=>#0=>#1=>#2",
    Ann(Lam(Lam(Lam(BVar(1)))),Pi(Type(),Pi(BVar(0),Pi(BVar(1),BVar(2))))))
  checkParse("(λλ#0 : *=>#0=>#1)", Ann(Lam(Lam(BVar(0))) ,Pi(Type(),Pi(BVar(0),BVar(1)))))
}
