package lambdaPi.main

import lambdaPi.syntax._
import lambdaPi.parser._
import lambdaPi.eval._
import lambdaPi.eval.evaluator._
import lambdaPi.typer._
import lambdaPi.typer.typer._

import cats.data.State


object Main extends scala.App {

  val parser = lambdaPiParser
  val env: Env = List()

  /*
   * Functions for extending the context 
   *    (which is within the TCB)
   */

  def addParsedDecl1(tcb: TCB, ids: String, tys: String): TCB = {
    val parseResE: Either[String, (Name, Term)] = for {
      id <- parser.parseName(ids)
      ty <- parser.parseTerm(tys)
    } yield (id, ty)
    parseResE match {
      case Right((id, ty)) => {
        val tyv = evalClosed(ty)
        tcb.pushDecl(id, tyv)
      }
      case Left(emsg) => {
        println(s"""- addParsedDecl failed: ${emsg}""")
        tcb
      }
    }
  }

  def addParsedDecl(ids: String, tys: String): State[TCB, Unit] =
    State(tcb => (addParsedDecl1(tcb, ids, tys), ()))

  val addTcbMon: State[TCB, Unit] = for {
    _ <- addParsedDecl("x", "*")
    _ <- addParsedDecl("Bool", "*")
    _ <- addParsedDecl("true",  "Bool")
    _ <- addParsedDecl("false", "Bool")
  } yield (())

  val ntcb: TCB = addTcbMon.run(new TCB()).value._1

  /*
   * Functions for printing the results of various functions
   *   pep: Parse and eval a string and print the result
   *   ptp: Parse and type a string and print the result
   */

  def pep (tms: String): Unit = {
    parser.parseTerm(tms) match {
      case Right(tm) => {
        val tmv = eval(env, tm);
        val tmvq = evalQuote(env, tm);
        println(s"""evalQuote("${tms}") = ${tmvq}""")
        // println(s"""    eval("${tms}") = $tmv""")
      }
      case Left(emsg) => println(s"""- Parsing of "${tms}" failed: ${emsg}""")
    }
  }

  def ptp (tms: String): Unit = {
    parser.parseTerm(tms) match {
      case Right(tm) => {
        typeTerm(ntcb, tm) match {
          case Ok(tyv) => println(s"""typeTerm("${tms}") = ${quote0(tyv)}""")
          case Err(msg, _) => println(s"""- Typing of "${tms}" failed: ${msg}""")
        }
      }
      case Left(emsg) => println(s"""- Parsing of "${tms}" failed: ${emsg}""")
    }
  }

  /*
   * Commands
   * --------
   */
  println("Pete's lambdaPi implementation:")
  println()

  pep("*");
  pep("λ#0");
  pep("*=>*");
  pep("λλ#0");
  pep("λλ#1");
  pep("*=>#0");
  pep("*=>(#0=>#1)");
  pep("x");
  println()

  ptp("*");
  ptp("x");
  ptp("λ#0 : *=>*");
  ptp("*=>*");
  //ptp("λλ#0");
  //ptp("λλ#1");
  ptp("*=>#0");
  ptp("*=>(#0=>#1)");
  println()

  println("assume Bool: *, true: Bool, false: Bool")
  ptp("Bool")
  ptp("true")
  ptp("(λ#0 : Bool=>Bool) true");
  ptp("(λλ#0 : *=>#0=>#1) Bool true");
  ptp("(λλ#0 : *=>#0=>#1) * true");
  println()

}
