package lambdaPi.typer

import lambdaPi.syntax._
import lambdaPi.eval._
import lambdaPi.eval.evaluator._

/*
 * Ctx: Context, a list of declarations
 */

class Ctx (val decls: List[(Name, TypeV)] = List()) {
  def pushDecl(n: Name, ty: TypeV) = new Ctx((n, ty) :: decls);
  def lookupTy(n: Name): Option[TypeV] = decls.find(_._1 == n) match {
    case Some(entry) => Some(entry._2)
    case None => None
  }
}


/*
 * TCB: Typer "control block": Contains context-level information the typer needs
 *   ctx: The context
 *   nBinders: how many binders have been seen
 */

class TCB (
  val ctx: Ctx = new Ctx(),
  val nBinders: Int = 0
) {
  def pushDecl(n: Name, ty: TypeV) = new TCB (ctx.pushDecl(n, ty), nBinders);
  def lookupTy(n: Name): Option[TypeV] = ctx.lookupTy(n);

  def pushBinder() = new TCB (ctx, nBinders+1);
  def getFreshLN(): (Name, TCB) = (NLocal(nBinders), new TCB (ctx, nBinders+1))
}

/*
 * TCM[A]: Type check monad: An error monad
 *   ret (A => TCM[A]): Return -- yield a result (in the monad)
 *   error ((String, Term) => TCM[A]): Yield an error
 */

trait TCM [+A] {
  def map [B] (f: A => B): TCM[B];
  def flatMap [B] (f: A => TCM[B]): TCM[B];
  def errorIf (b: A => Boolean, err: String, t: Term): TCM[A];
};

case class Ok[A] (v: A) extends TCM[A] {
  def map [B] (f: A => B) = Ok(f(v));
  def flatMap [B] (f: A => TCM[B]) = f(v);
  def errorIf (b: A => Boolean, err: String, t: Term) =
    if (b(v)) this else Err(err, t)
};
case class Err[A] (err: String, t: Term) extends TCM[A] {
  def map [B] (f: A => B) = Err(err, t);
  def flatMap [B] (f: A => TCM[B]) = Err(err, t);
  def errorIf (b: A => Boolean, err: String, t: Term) = this;
};

object TCM {
  def ret[A] (ret: A): TCM[A] = Ok(ret);
  def error[A] (err: String, t: Term): TCM[A] = Err(err,t);
}

// Alternate implementation
// type TCM [R] = Either[(String, Term), R];
// def ret[R] (ret: R): TCM[R] = Right(ret);
// def error[R] (err: String, t: Term): TCM[R] = Left((err,t));

/*
 * 
 * typer: The object containing the type functions
 * ------------------------------------------------
 * 
 */

object typer {
  import TCM._

  val typerDebug: Boolean = false

  /*
   *  typeTerm: synthesize the type of a term
   */
  def typeTerm (tcb: TCB, e: Term): TCM[TypeV] = {
    if (typerDebug) {
      println(s"""\n[trace] typeTerm: "${syntaxOps.showc(e)}"""")
    }
    typeTerm0(tcb, e)
  }

  def typeTerm0 (tcb: TCB, e: Term): TCM[TypeV] = {
    if (typerDebug) {
      val posStr = e.pos.toString match {
        case "<undefined position>" => "   "
        case s => s"@$s "
      }
      println(s"""[trace] ${posStr} \ttype : ${syntaxOps.showc(e)}""")
      val res = typeTerm1(tcb, e)
      res match {
        case Ok(v) => println(s"[trace] \ttypeTerm returned: ${evalOps.showcn(tcb.nBinders, v)}")
        case Err(msg, etm) => println(s"[trace] \ttypeTerm: FAILED")
      }
      res
    } else typeTerm1(tcb, e)
  }

  def typeTerm1 (tcb: TCB, e: Term): TCM[TypeV] = e match {
    case Ann(e2, expTy) => for {
      _ <- checkTerm0(tcb, expTy, VType())
      expTV = evalClosed(expTy)
      _ <- checkTerm0(tcb, e2, expTV)
    } yield expTV;

    case Type() => ret(VType());

    case Pi(dom, range) => for {
      _ <- checkType0(tcb, dom)
      domV = evalClosed(dom)
      (lName, nTcb) = tcb.getFreshLN()
      srange = range.subst0(FVar(lName))
      _ <- checkType0(nTcb.pushDecl(lName, domV), srange)
    } yield VType()

    case BVar(_) => error("Bound variable in term", e);

    case FVar(n) => tcb.lookupTy(n) match {
      case Some(ty) => ret(ty)
      case None => error(s"Undefined variable: $n", e);
    }

    case App(func, arg) => for {
      funcTy <- typeTerm0(tcb, func)
      rTy <- funcTy match {
        case VPi(domTy, rangeTF) => for {
          _ <- checkTerm0(tcb, arg, domTy)
        } yield rangeTF(evalClosed(arg))
        case _ => error(s"Application of non-Pi type, ${funcTy}, for ${func}", e);
      }
    } yield rTy

    case Lam(body) => error(s"Untyped lambda", e);

  }

  /*
   *  checkTerm (and checkType): check that a term has a given type
   */

  def checkTerm (tcb: TCB, e: Term, expTyV: TypeV): TCM[Unit] = {
    if (typerDebug) {
      println(s"""\n[trace] checkTerm: "${syntaxOps.showc(e)}"""")
     }
    checkTerm0(tcb, e, expTyV)
  }

  def checkType0 (tcb: TCB, e: Term): TCM[Unit] = checkTerm0(tcb, e, VType())

  def checkTerm0 (tcb: TCB, e: Term, expTyV: TypeV): TCM[Unit] = {
    if (typerDebug) {
      val posStr = e.pos.toString match {
        case "<undefined position>" => ""
        case s => s"@$s "
      }
      println(s"""[trace] ${posStr} \tcheck: ${syntaxOps.showc(e)} : ${evalOps.showcn(tcb.nBinders,expTyV)}""")
    }
    checkTerm1(tcb, e, expTyV)
  }

  def checkTerm1 (tcb: TCB, e: Term, expTy: TypeV): TCM[Unit] = e match {

    case Lam(body) => expTy match {
      case VPi(td, ftr) => {
        val (lName, nTcb) = tcb.getFreshLN();
        val sbody = body.subst0(FVar(lName));
        checkTerm0(nTcb.pushDecl(lName, td), sbody, ftr(vfree(lName)));
      }
      case _ => error(s"Type of lambda not a Pi type: ${expTy}", e);
    }

    // for everything else, use type synthesis and check return
    case _ => for {
      actTy <- typeTerm1(tcb, e);
      _ <- checkTyEq(actTy, expTy, e)
    } yield ()
  }

  /*
   *  checkTyEq: check that two types are equal
   */

  def checkTyEq (actTy: TypeV, expTy: TypeV, t: Term): TCM[Unit] = {
    val actTyS = quote0(actTy)
    val expTyS = quote0(expTy)
    if (actTyS == expTyS) ret(())
    else error(s"Inferred type ($actTyS}) != expected type (${expTyS})", t)
  }
}

