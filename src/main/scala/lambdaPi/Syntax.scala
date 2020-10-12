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
 *  Name ::= 
 *   NGlobal (n: String)
 *   NLocal (nbinders: Int)
 *   NQuote (n: Int)
 */

trait Term {
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
