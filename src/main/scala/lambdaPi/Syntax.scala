package lambdaPi.syntax

/*
 * Term ::= 
 *   Ann (tm: Term, ty: Term)
 *   Type ()
 *   Pi (dom: Term, range: Term)
 *   BVar (nbinders: Int)
 *   FVar (name: Name) 
 *   App (func: Term, arg: Term)
 *   Lam (body: Term) 
 * 
 * Name ::= 
 *   NGlobal (n: String)
 *   NLocal (nbinders: Int)
 *   NQuote (n: Int)
 */

trait Term extends scala.util.parsing.input.Positional {
  def subst (i: Int, r: Term): Term;
  def subst0 (r: Term): Term = this.subst(0, r);
}
case class Ann (tm: Term, ty: Term)  extends Term {
  def subst (i: Int, r: Term): Term = Ann(tm.subst(i, r), ty.subst(i, r));
}
case class Type ()                     extends Term {
  def subst (i: Int, r: Term): Term = this;
};
case class Pi (dom: Term, range: Term) extends Term {
  def subst (i: Int, r: Term): Term = Pi(dom.subst(i, r), range.subst(i+1, r));
};
case class BVar (nbinders: Int)        extends Term {
  def subst (i: Int, r: Term): Term = if (i==nbinders) r else BVar(nbinders)
};
case class FVar (name: Name)           extends Term {
  def subst (i: Int, r: Term): Term = this
};
case class App (func: Term, arg: Term) extends Term {
  def subst (i: Int, r: Term): Term = App(func.subst(i, r), arg.subst(i, r));
};
case class Lam (body: Term)            extends Term {
  def subst (i: Int, r: Term): Term = Lam (body.subst(i+1, r))
};

trait Name
case class NGlobal (n: String)    extends Name {
  override def toString = s"""NGlobal(\"${n}\")""";
}
case class NLocal (nbinders: Int) extends Name;
case class NQuote (n: Int)        extends Name;

object syntaxOps {

  /*
   * Show concise
   *   showcm (m, t): show t, parenthesized if its prec < m (min)
   *   showcp (t): yield (p, s), where p is t's level and s is the string
   */
  def showc (t: Term): String = showcm(0, t)
  def showcm(m: Int, t: Term) = {
    val (p, s) = showcp(t);
    if (p < m) s"(${s})" else s
  }

  def showcp (t: Term): (Int, String) = t match {
    case Ann (tm: Term, ty: Term) =>    (3, s"${showcm(5, tm)} : ${showcm(5, ty)}");
    case Type () =>                     (9, "*");
    case Pi (dom: Term, range: Term) => (6, s"${showcm(7, dom)} => ${showcm(6, range)}");
    case BVar (nbinders: Int) =>        (9, s"#${nbinders}");
    case FVar (name: Name) =>           (9, showc(name));
    case App (func: Term, arg: Term) => (7, s"${showcm(9, func)} ${showcm(9, arg)}");
    case Lam (body: Term) =>            (5, s"λ${showcm(5, body)}");
  }

  def showc (n: Name): String = n match {
    case NGlobal (n: String) =>    n;
    case NLocal (k: Int) =>        s"#$k"
    case NQuote (k: Int) =>        s"'k";
  }

  def showcn (nb: Int, n: Name): String = n match {
    case NGlobal (n: String) =>    n;
    case NLocal (k: Int) =>        s"#${nb-k-1}"
    case NQuote (k: Int) =>        s"'k";
  }

}
