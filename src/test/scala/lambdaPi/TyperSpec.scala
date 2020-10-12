package lambdaPi.test

import org.scalatest._
import flatspec._
import matchers._

import lambdaPi.syntax._
import lambdaPi.parser._
import lambdaPi.eval._
import lambdaPi.eval.evaluator._
import lambdaPi.typer._
import lambdaPi.typer.typer._

class TyperSpec extends AnyFlatSpec with should.Matchers {

  val parser = lambdaPiParser
  val tcb: TCB = new TCB()

  def parseChecked (s: String): Term = lambdaPiParser.parseTerm(s) match {
    case Right(tm) => tm
    case Left(emsg) => fail(s"""parsing of ${s}"failed: ${emsg}""")
  }

  def parseNameChecked (s: String): Name = parseChecked(s) match {
    case FVar(n) => n
    case x => fail(s"""The term${x} is not a name""")
  }


  def checkTypeTerm (tcb: TCB, tms: String, expTyS: String = null) = {
    it should s"TYPE term ${tms} to ${expTyS}" in {
      val tm = parseChecked(tms)
      val expTy = if (expTyS == null) null else parseChecked(expTyS)
      // val expTyV = evalClosed(expTy);
      val actTyV = typeTerm(tcb, tm) match {
        case Ok(tyv) => tyv
        case Err(msg, tm) => fail(s"""Failed typing ${tm}: ${msg}""")
      }
      val actTy = quote0(actTyV)
      if (expTy == null) {
        info(s"Typed to: ${actTy}")
        fail("Expected type not specified")
      }
      else actTy should be (expTy)
    }
  }

  def checkTypeTerm2 (tcb: TCB, tms: String, expTy: TypeV = null) = {
    it should s"type term ${tms} to ${expTy}" in {
      val tm = parseChecked(tms)
      val ty = typeTerm(tcb, tm) match {
        case Ok(ty) => ty
        case Err(msg, tm) => fail(s"""Failed typing ${tm}: ${msg}""")
      }
      if (expTy == null) info(s"Typed to: ${ty}")
      else ty should be (expTy)
    }
  }

  def checkCheckTerm (tcb: TCB, tms: String, expTys: String) = {
    it should s"CHECK that term ${tms} has type ${expTys}" in {
      val tm = parseChecked(tms);
      val expTy = parseChecked(expTys);
      val expTyV = evalClosed(expTy);
      checkTerm(tcb, tm, expTyV) match {
        case Ok(_) => succeed
        case Err(msg, tm) => fail(s"""Failed typing ${tm}: ${msg}""")
      }
      // if (expTy == null) info(s"Typed to: ${ty}")
      // else ty should be (expTy)
    }
  }

    def checkCheckTerm2 (tcb: TCB, tms: String, expTy: TypeV) = {
    it should s"check that term ${tms} has type ${expTy}" in {
      val tm = parseChecked(tms);
      checkTerm(tcb, tm, expTy) match {
        case Ok(_) => succeed
        case Err(msg, tm) => fail(s"""Failed typing ${tm}: ${msg}""")
      }
      // if (expTy == null) info(s"Typed to: ${ty}")
      // else ty should be (expTy)
    }
  }


  behavior of "The typer functions typeTerm and checkTerm:"

  checkTypeTerm(tcb, "*", "*")
  checkCheckTerm(tcb, "*", "*")
  checkCheckTerm(tcb, "*=>#0=>#1=>#2", "*")
  checkCheckTerm(tcb, "λλλ#1", "*=>#0=>#1=>#2")
  checkCheckTerm(tcb, "λλλ#0", "*=>#0=>#1=>#2")
  checkTypeTerm(tcb, "(λλλ#1 : *=>#0=>#1=>#2)", "*=>#0=>#1=>#2")
  checkTypeTerm(tcb, "(λλλ#1 : *=>#0=>#1=>#2) (*=>#0=>#1)", "(*=>#0=>#1)=>(*=>#0=>#1)=>(*=>#0=>#1)")
  // TBD: checkTypeTerm(tcb, "(λλλ#1 : *=>#0=>#1=>#2) (*=>#0=>#1) (λλ#1)")//, "(*=>#0=>#1)=>(*=>#0=>#1)")

  checkCheckTerm(tcb, "λ#0", "*=>*")
  checkCheckTerm(tcb, "λλ#0", "*=>#0=>#1")
  checkCheckTerm(tcb, "(λλ#0 : *=>#0=>#1)*", "*=>*")
  checkCheckTerm(tcb, "(λλ#0 : *=>#0=>#1)(*=>#0=>#1)", "(*=>#0=>#1)=>(*=>#0=>#1)")
  checkCheckTerm(tcb, "(λλ#0 : *=>#0=>#1)(*=>#0=>#1)(λλ#0)", "(*=>#0=>#1)")

  checkCheckTerm(tcb, "(λ#0 : *=>*)(*=>#0=>#1=>#2)", "*")
  checkCheckTerm(tcb, "(λλ#0 : *=>#0=>#1)(*=>#0=>#1=>#2)", "(*=>#0=>#1=>#2)=>(*=>#0=>#1=>#2)")
  checkCheckTerm(tcb, "(λλλ#0 : *=>#0=>#1=>#2)(*=>#0=>#1=>#2)(λλλ#1)(λλλ#0)", "(*=>#0=>#1=>#2)")
  checkCheckTerm(tcb, "(λλλ#1 : *=>#0=>#1=>#2)(*=>#0=>#1=>#2)(λλλ#1)(λλλ#0)", "(*=>#0=>#1=>#2)")

  val ntcb = tcb
    .pushDecl(parseNameChecked("x"), VType())
    .pushDecl(parseNameChecked("Bool"), VType())
    .pushDecl(parseNameChecked("true"), VNeutral(NFree(NGlobal("Bool"))))
    .pushDecl(parseNameChecked("false"), VNeutral(NFree(NGlobal("Bool"))))

  info("Context extended:")
  checkTypeTerm(ntcb, "x", "*")
  checkTypeTerm(ntcb, "Bool", "*")
  checkTypeTerm(ntcb, "true", "Bool")
  checkCheckTerm(ntcb, "false", "Bool")
  // more TBD

}
