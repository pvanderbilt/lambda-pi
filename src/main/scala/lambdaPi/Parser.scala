package lambdaPi.parser

import lambdaPi.syntax._
import scala.util.parsing.combinator._

object lambdaPiParser extends RegexParsers {

  def term:  Parser[Term] = term3

  // Annotation
  def term3: Parser[Term] =
    term5 ~ ":" ~ term5 ^^ { case term ~ _ ~ ty => Ann(term, ty) } | term5;

  // Lambda
  def term5: Parser[Term] = "λ" ~ term5 ^^ { case _ ~ body => Lam(body) } | term6;

  // Sigma (=>)
  def term6: Parser[Term] =
    term7 ~ "=>" ~ term6 ^^ { case dom ~ _ ~ range => Pi(dom, range) } | term7;

  // Application
  def term7: Parser[Term] =
    term9 ~ rep(term9) ^^ { case func ~ args => args.foldLeft(func)(App) }; 

  // Variables and *; also parenthesised
  def term9: Parser[Term] = id | bvar | star | grp;

  def id:    Parser[Term] = """[a-zA-Z]+""".r          ^^ { s => FVar(NGlobal(s)) }
  def bvar:  Parser[Term] = "#" ~ """(0|[1-9]\d*)""".r ^^ { case _ ~ ns => BVar(ns.toInt) }
  def star:  Parser[Term] = "*"                        ^^ { _ => Type() }
  def grp:   Parser[Term] = "(" ~> term <~ ")"


  // Parsing functions
  def parseTermRaw(s: String) = parseAll[Term](term, s)
  def parseTerm(s: String): Either[String, Term] = parseTermRaw(s) match {
    case Success(r,_) => Right(r)
    case NoSuccess(msg, _) => Left(s"NoSuccess: ${msg}")
    case Error(msg, _) => Left(s"Error: ${msg}")
  }
}

