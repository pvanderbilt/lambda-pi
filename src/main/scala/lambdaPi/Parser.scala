package lambdaPi.parser

import lambdaPi.syntax._
import scala.util.parsing.combinator._

object lambdaPiParser extends RegexParsers {

  def term:  Parser[Term] = positioned(term3)

  // Annotation
  def term3: Parser[Term] =
    positioned(term5 ~ ":" ~ term5 ^^ { case term ~ _ ~ ty => Ann(term, ty) }) | term5;

  // Lambda
  def term5: Parser[Term] =
    positioned("λ" ~ term5 ^^ { case _ ~ body => Lam(body) } | term6);

  // Pi (=>)
  def term6: Parser[Term] =
    positioned(term7 ~ "=>" ~ term6 ^^ { case dom ~ _ ~ range => Pi(dom, range) }) | term7;

  // Application
  def term7: Parser[Term] =
    positioned(term9 ~ rep(term9) ^^ { case func ~ args => args.foldLeft(func)(App) }); 

  // Variables and *; also parenthesised
  def term9: Parser[Term] = positioned(id | bvar | star | grp);

  def id:    Parser[Term] = """[a-zA-Z]+""".r          ^^ { s => FVar(NGlobal(s)) }
  def bvar:  Parser[Term] = "#" ~ """(0|[1-9]\d*)""".r ^^ { case _ ~ ns => BVar(ns.toInt) }
  def star:  Parser[Term] = "*"                        ^^ { _ => Type() }
  def grp:   Parser[Term] = "(" ~> term <~ ")"


  // Parsing functions
  def parseTermRaw (s: String) = parseAll[Term](term, s)
  def parseTerm (s: String): Either[String, Term] = parseTermRaw(s) match {
    case Success(r,_) => Right(r)
    case NoSuccess(msg, _) => Left(s"NoSuccess: ${msg}")
    case Error(msg, next) => Left(s"Error: ${msg} (@$next)")
  }

  def parseName (s: String): Either[String, Name] = parseTerm(s) match {
    case Right(tm) => tm match {
      case FVar(n) => Right(n)
      case _ => Left(s"""The term ${s} is not a name""")
    }
    case Left(emsg) => Left(emsg);
  }
}

