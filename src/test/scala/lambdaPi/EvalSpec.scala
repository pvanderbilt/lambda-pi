package lambdaPi.test

import org.scalatest._
import flatspec._
import matchers._

import lambdaPi.syntax._
import lambdaPi.parser._
import lambdaPi.eval._
import lambdaPi.eval.evaluator._

//import ParserSpec.parseChecked

class EvalSpec extends AnyFlatSpec with should.Matchers {

  val parser = lambdaPiParser
  // val parserChecking = parser2.parseChecked
  val tm = "*"
  val env: Env = List()

  // s"Eval(${tm})" should "yield VType()" in {
  //   val rptm = parser.parseTermRaw(tm)
  //   val ptm = rptm match {
  //     case parser.Success(ast, _) => ast
  //     case parser.NoSuccess(msg, _) => fail(s"NoSuccess when parsing  '${tm}'; msg = ${msg}")
  //     case parser.Error(msg, _) => fail(s"Error when parsing  '${tm}'; msg = ${msg}")
  //   }
  //   ptm should be (Type())
  //   val vtm = eval(env, ptm)
  //   vtm should be (VType())
  // }

  def parseChecked (s: String): Term = lambdaPiParser.parseTerm(s) match {
    case Right(tm) => tm
    case Left(emsg) => fail(s"""parsing of ${s}"failed: ${emsg}""")
  }

  "The eval function" should "exist" in { assert(eval _ != null) }

  def checkEval (env: Env, tm: Term, expVal: Value) = {
    it should s"map term ${tm} to ${expVal}" in {
      val vtm = eval(env, tm)
      vtm should be (expVal)
    }
  }

  def checkEvalS (env: Env, stm: String, expVal: Value) = {
    it should s"""map parse of "${stm}" to ${expVal}""" in {
      val tm = parseChecked(stm);
      // val rptm = parser.parseTermRaw(stm)
      // val tm = rptm match {
      //   case parser.Success(ast, _) => ast
      //   case parser.NoSuccess(msg, _) => fail(s"NoSuccess when parsing  '${stm}'; msg = ${msg}")
      //   case parser.Error(msg, _) => fail(s"Error when parsing  '${stm}'; msg = ${msg}")
      // }
      // ptm should be (Type())
      val vtm = eval(env, tm)
      vtm should be (expVal)
    }
  }

  def checkEvalQuote (env: Env, stm: String) = {
    val tm = parseChecked(stm);
    // val rptm = parser.parseTermRaw(stm)
    // val tm = rptm match {
    //   case parser.Success(ast, _) => ast
    //   case parser.NoSuccess(msg, _) => fail(s"NoSuccess when parsing  '${stm}'; msg = ${msg}")
    //   case parser.Error(msg, _) => fail(s"Error when parsing  '${stm}'; msg = ${msg}")
    // }
    it should s"""map "${stm}" to itself \t(${tm})""" in {
      // info(s"  tm = ${tm}");
      val ntm = evalQuote(env, tm)
      ntm should be (tm)
    }
  }

  def checkEvalQuoteExp (env: Env, stm: String, sExpTm: String = null) = {
    val tm = parseChecked(stm);
    // val rptm = parser.parseTermRaw(stm)
    // val tm = rptm match {
    //   case parser.Success(ast, _) => ast
    //   case parser.NoSuccess(msg, _) => fail(s"NoSuccess when parsing  '${stm}'; msg = ${msg}")
    //   case parser.Error(msg, _) => fail(s"Error when parsing  '${stm}'; msg = ${msg}")
    // }
    val expTm: Term = if (sExpTm == null) null else parseChecked(sExpTm);
    it should s"""map "${stm}" to "${sExpTm}"""" in {
      // info(s"  tm = ${tm}");
      //  \t(term: ${tm})
      val ntm = evalQuote(env, tm)
      ntm should be (expTm)
    }
  }


  checkEval(env, FVar(NGlobal("x")), VNeutral(NFree(NGlobal("x"))));

  checkEvalS(env, "*", VType());
  checkEvalS(env ++ List(VType()), "#0", VType());
  checkEvalS(env, "y",  VNeutral(NFree(NGlobal("y"))));
  checkEvalS(env, "(λ#0)*", VType());

  // "The evalQuote function" should "exist" in { assert(evalQuote _ != null) }
  info("The evalQuote function");

  // These things (with λ or =>) yield functions which can't be compared
  checkEvalQuote(env, "λ#0");
  checkEvalQuote(env, "*=>*");
  checkEvalQuote(env, "λλ#0");
  checkEvalQuote(env, "λλ#1");
  checkEvalQuote(env, "*=>#0");
  checkEvalQuote(env, "*=>(#0=>#1)");
  checkEvalQuote(env, "x");

  // These things don't eval to themselves (they have a beta-reduction)
  checkEvalQuoteExp(env, "(λ#0)*", "*");
  checkEvalQuoteExp(env, "(λλ#0) x y", "y");
  checkEvalQuoteExp(env, "(λλ #1) x y", "x");
  checkEvalQuoteExp(env, "(λλλ#0) (*=>#0=>#1)", "λλ#0")
  checkEvalQuoteExp(env, "(λλλ#0) (*=>#0=>#1) (λλ#1)", "λ#0")
  checkEvalQuoteExp(env, "(λλλ#0) (*=>#0=>#1) (λλ#1) (λλ#0)", "λλ#0")
  checkEvalQuoteExp(env, "(λλλ#1 : *=>#0=>#1=>#2)","λλλ#1")
  checkEvalQuoteExp(env, "(λλλ#1) (*=>#0=>#1)", "λλ#1")
  checkEvalQuoteExp(env, "(λλλ#1) (*=>#0=>#1) (λλ#1)", "λλλ#1")
  checkEvalQuoteExp(env, "(λλλ#1) (*=>#0=>#1) (λλ#1) (λλ#0)", "λλ#1")
  // checkEvalQuoteExp(env, "(λλλλ#3) (*=>#0=>#1) (λλ#1) (λλ#0)", "λλ#1")
}
