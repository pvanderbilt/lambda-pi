package lambdaPi.eval

import lambdaPi.syntax._

/*
 * Value ::=
 *   VLam (bf: Value => Value)
 *   VType () 
 *   VPi  (dom: Value, range: Value => Value)
 *   VSig (dom: Value, range: Value => Value)
 *   VNeutral (n: Neutral)
 * 
 *  Neutral ::=
 *   NFree (name: Name)
 *   NApp (func: Neutral, arg: Value) 
 */

trait Value;
case class VLam (bf: Value => Value)                extends Value;
case class VType ()                                 extends Value;
case class VPi (dom: Value, range: Value => Value)  extends Value;
case class VSig (dom: Value, range: Value => Value) extends Value;
case class VPair (v0: Value, v1: Value)             extends Value;
case class VNeutral (n: Neutral)                    extends Value;

trait Neutral;
case class NFree (name: Name)               extends Neutral;
case class NApp (func: Neutral, arg: Value) extends Neutral;
case class NPair (tpl: Neutral, indx: Int)  extends Neutral;

object evaluator {
  type TypeV = Value;
  type Env = List[Value];

  def eval (env: Env, t: Term): Value = t match {
    case Ann(t, _)     => eval(env, t);
    case Type ()       => VType();
    case Pi (d, r)     => VPi(eval(env, d), (x => eval(x :: env, r)));
    case Sigma (d, r)  => VSig(eval(env, d), (x => eval(x :: env, r)));
    case BVar (nb)     => env(nb);
    case FVar (n)      => vfree(n);
    case App (f, a)    => apply(eval(env, f), eval(env, a));
    case Lam (b)       => VLam(x => eval(x :: env, b))
    case Pair(t0, t1)  => VPair(eval(env, t0), eval(env, t1))
    case Proj(t, indx) => select(eval(env, t), indx)
  }

  def apply (f: Value, a: Value): Value = f match {
    case VLam(bf)    => bf(a);
    case VNeutral(n) => VNeutral(NApp(n, a));
  }

  def select (t: Value, indx: Int): Value = t match {
    case VPair(v0, v1) => if (indx == 0) v0 else v1;
    case VNeutral(n)   => VNeutral(NPair(n, indx));
  }

  def vfree (n: Name): Value = VNeutral(NFree(n));

  def evalClosed (t: Term): Value = eval(List(), t);

  def evalQuote (env: Env, t: Term): Term = quote0(eval(env,t));

  def quote0 (v: Value): Term = quote(0, v);

  def quote (i: Int, v: Value): Term = v match {
    case VLam (bf) => Lam(quote(i+1, bf(vfree(NQuote(i)))));
    case VType () => Type();
    case VPi (dom, frange)  => Pi(quote(i, dom), quote(i+1, frange(vfree(NQuote(i))))) ;
    case VSig (dom, frange) => Sigma(quote(i, dom), quote(i+1, frange(vfree(NQuote(i))))) ;
    case VPair(v0, v1) => Pair(quote(i, v0), quote(i, v1))
    case VNeutral (n) => neutralQuote(i, n);
  }

  def neutralQuote (i: Int, n: Neutral): Term = n match {
    case NFree (name) => boundFree(i, name);
    case NApp (nfunc, arg) => App(neutralQuote(i, nfunc), quote(i, arg));
  }

  def boundFree (i: Int, n: Name): Term = n match {
    case NQuote(k) => BVar(i-k-1)
    case x => FVar(x)
  }
}

object evalOps {

  def showc (v: Value): String = showcn (0, v)

  def showcn (nb: Int, v: Value): String = v match {
    case VLam (bf) => {
      val bs = bf(VNeutral(NFree(NLocal(nb))))
      s"λ[${showcn(nb+1, bs)}]";
    };
    case VType () => s"*";
    case VPi (dom, rf) => {
      val bs = rf(VNeutral(NFree(NLocal(nb))))
      s"${showcn(nb, dom)}=>[${showcn(nb+1, bs)}]";
    }
    case VSig (dom, rf) => {
      val bs = rf(VNeutral(NFree(NLocal(nb))))
      s"${showcn(nb, dom)}*[${showcn(nb+1, bs)}]";
    }
    case VNeutral (n) => s"${showcn(nb, n)}";
  }

  def showcn (nb: Int, n: Neutral): String = n match {
    case NFree (name) => syntaxOps.showcn(nb, name);
    case NApp (func, arg) => s"${showcn(nb, func)} ${showcn(nb, arg)}";
  }
}

