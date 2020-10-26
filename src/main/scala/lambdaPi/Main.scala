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
   *   pes: Parse and eval a string and print the result (using showc)
   *   pts: Parse and type a string and print the result (using showc)
   *   peq: Parse and eval a string and print its quoted result
   *   ptq: Parse and type a string and print its quoted result
   */

  def pes (tms: String): Unit = {
    parser.parseTerm(tms) match {
      case Right(tm) => {
        val tmv = eval(env, tm);
        val tmvs = evalOps.showc(tmv);
        val tmvq = evalQuote(env, tm); //  \t(${tmvq})
        println(s"""eval("${tms}") = $tmvs""")
      }
      case Left(emsg) => println(s"""- Parsing of "${tms}" failed: ${emsg}""")
    }
  }

  def pts (tms: String): Unit = {
    parser.parseTerm(tms) match {
      case Right(tm) => {
        typeTerm(ntcb, tm) match {
          case Ok(tyv) => println(s"""typeTerm("${tms}") = ${evalOps.showc(tyv)}""")
          case Err(msg, etm) => {
            println(s"""typeTerm("${tms}") = ...""")
            println(s"""[error] "${tms}"@${etm.pos}: ${msg}""")
            // println(s"""    @"${syntaxOps.showc(etm)}" """)
            etm.pos.longString.linesIterator.foreach {
              str => println("[error]     |" ++ str)
            }
            //println(etm.pos.longString)
          }
        }
      }
      case Left(emsg) => println(s"""- Parsing of "${tms}" failed: ${emsg}""")
    }
  }

  def peq (tms: String): Unit = {
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

  def ptq (tms: String): Unit = {
    parser.parseTerm(tms) match {
      case Right(tm) => {
        typeTerm(ntcb, tm) match {
          case Ok(tyv) => println(s"""typeTerm("${tms}") = ${quote0(tyv)}""")
          case Err(msg, t) => {
            println(s"""- Typing of "${tms}" failed: ${msg}""")
            println(s"""    @"$t"""")
          }
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

  pes("*");
  pes("λ#0");
  pes("*=>*");
  pes("λλ#0");
  pes("λλ#1");
  pes("λλλλ#0 #1 #2 #3");
  pes("*=>#0");
  pes("*=>(#0=>#1)");
  pes("Bool");
  pes("true");
  pes("(λλ#0) true");
  pes("(λλ#0) true false");
  pes("(λ#0)(λ#0)");
  pes("(λλ#0)(Bool=>Bool)(λ#0)");
  println()

  pts("*");
  pts("λ#0 : *=>*");
  pts("*=>*");
  pts("λλ#0");
  pts("λλ#0 : *=>#0=>#1");
  pts("*=>#0");
  pts("*=>(#0=>#1)");
  println()

  println("assume Bool: *, true: Bool, false: Bool")
  pts("Bool")
  pts("true")
  pts("(λ#0 : Bool=>Bool) true");
  pts("(λλ#0 : *=>#0=>#1) Bool true");

  val lpf_id = "(λλ#0 : *=>#0=>#1)"
  println(s"\ndefine id = $lpf_id");
  pts(lpf_id);
  // pts(s"$lpf_id Bool");
  // pts(s"$lpf_id Bool true");
  // pts(s"$lpf_id * Bool");
  // pts(s"$lpf_id * Bool");
  // pts(s"$lpf_id ($lpf_id * Bool) true");
  // pes(s"$lpf_id ($lpf_id * Bool) true");
  // pes(s"$lpf_id ($lpf_id * Bool)");

  // // const : (A:*) => A => ((B:*) => B) => A = λA. λa. λB. λb. a
  // val lpf_const = "(λλλλ#2 : *=>#0=>*=>#0=>#2)"
  // println(s"\ndefine const = $lpf_const");
  // pts(lpf_const);
  // pes(lpf_const);

  // pts("true *");
  // pts("(λλ#0 : *=>#0=>#1) * true");
  // println()

}
