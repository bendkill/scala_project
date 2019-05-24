import ast._

object Evaluator {
  def castErr (tau: ty.Ty) : Term = {
    println(s"cast error: $tau")
    Empty
  }

  def termErr (t: Term) : Term = {
    println(s"eval error: $t")
    Empty
  }

  def err (msg: String) : Term = {
    pritnln(s"eval error: %msg")
    Empty
  }

  def cast (t: Term, tau:ty.Ty) : Term = tau match {
    case ty.Natural => t match {
      case Zero | Succ(_) => t
      case Integer(False | True, Zero) => Zero
      case Integer(False, t2) => t2
      case Rational(Integer(False | True, Zero), _) => Zero
      case Rational(Integer(False, t2), Succ(Zero)) => t2
      case _ => castErr(tau)
    }
    case ty.Integer => t match {
      case Zero | Succ(_) => Integer(False, t)
      case Integer(_, _) => t
      case Rational(t1, Succ(Zero)) => t1
      case _ => castErr(tau)
    }
    case ty.Rational => t match {
      case Zero | Succ(_) => Rational(Integer(False, t), Succ(Zero))
      case Integer(_, _) => Rational(t, Succ(Zero))
      case Rational(_, _) => t
      case _ => castErr(tau)
    }
    case _ => castErr(tau)
  }

  def negate (v: Term) : Term = v match {
    case Zero => Integer(False, Zero)
    case Succ(v1) => Integer(True, Succ(v1))
    case Integer(False, v1) => Integer(True, v1)
    case Integer(True, v1) => Integer(False, v1)
    case Rational(Integer(False, v1), v2) => Rational(Integer(True, v1), v2)
    case Rational(Integer(True, v1), v2) => Rational(Integer(False, v1), v2)
    case _ => termErr(t)
  }

  def positive (v:Term) : Term = v match {
    case Zero => Integer(False, Zero)
    case Succ(v1) => Integer(False, Succ(v1))
    case Integer(v1, v2) => Integer(v1, v2)
    case Rational(v1, v2) => Rational(v1, v2)
    case _ => termErr(t)
  }

  def not(v: Term) : Term = v match {
    case True => False
    case False => True
    case _ => err(s"Not($t)")
  }

  def even(v: Term) : Term = v match {
    case Zero => True
    case Succ(Zero) => False
    case Succ(v1) => not(even(v1))
    case Integer(_, v2) => even(v2)
    case Rational(_, _) => err(s"Even($t)")
    case _ => err(s"Even($t)")
  }

  def odd(t: Term) : Term = not(even(t))

  // TODO: maybe don't need type for add, as long as both terms already values
  // and properly typed, following conventions for other helpers
  def add(t1: Term, t2: Term, tau: ty.Ty) : Term = tau match {
      case ty.Natural => cast(this(t1), tau) match {
        case Zero => cast(this(t2), tau)
        case Succ(v1) => add(v1, Succ(t2), tau)
        case _ => errMsg("Add")
      }
      case ty.Integer => cast(this(t1), tau) match {
        case Integer(False | True, Zero) => cast(this(t2), tau)
        case Integer(False, Succ(v1)) => cast(this(t2), tau) match {
          case Integer(True | False, Zero) => Integer(False, Succ(v1))
          case Integer(False, v2) =>
            add(Integer(False, v1), Integer(False, Succ(v1)), tau)
          case Integer(True, Succ(v2)) =>
            add(Integer(False, v1), Integer(True, v2), tau)
          case _ => errMsg("Add")
        }
        case Integer(True, Succ(v1)) => cast(this(t2), tau) match {
          case Integer(True | False, Zero) => Integer(True, Succ(v1))
          case Integer(False, Succ(v2)) =>
            add(Integer(True, v1), Integer(False, v2), tau)
          case Integer(True, v2) =>
            add(Integer(True, v1), Integer(True, Succ(v2)), tau)
          case _ => errMsg("Add")
        }
        case _ => errMsg("Add")
      }
      case ty.Rational => cast(this(t1), tau) match {
        case Rational(Integer(False | True, Zero), _) => cast(this(t2), tau)
        case _ => errMsg("TODO") // TODO 
      }
      case _ => errMsg("unchecked type")
    }

  // Quotient, Remainder
  def divByTwoNat(v: Term) : (Term, Term) = v match {
    case Zero => (Zero, Zero)
    case Succ(Zero) => (Zero, Succ(Zero))
    case Succ(Succ(v1)) =>
      val (q, r) = divByTwoNat(v1)
      (Succ(q), r)
    case _ => err("divByTwoNat unchecked type")
  }

  def multiply (v1: Term, v2: Term, tau: ty.Ty) : Term = tau match {
    // assumes same type,
    case ty.Natural => (v1, v2) match {
      case (Zero, _) => Zero
      case (_, Zero) => Zero
      case (Succ(Zero), v2) => v2
      case (Succ(v1), v2) => v2
    }
    case ty.Integer =>
    case ty.Rational =>
  }

  def gcd(v1: Term, v2: Term) : Term = if (v1 == Zero) v2
    else (divByTwoNat(v1), divByTwoNat(v2)) match {
    case ((q1, Zero), (q2, Zero)) => 
    case 

  }

  def apply (t: Term) : Term = t match {
    case Empty => Empty
    case True => True
    case False => False
    case Zero => Zero
    case Succ(t1) => Succ(this(t1))
    case Pred(Zero) => Zero
    case Pred(Succ(t1)) => this(t1)
    case Integer(t1, t2) => Integer(this(t1), this(t2))
    case Rational(t1, t2) => Rational(this(t1), this(t2))
    case Negate(t1) => negate(this(t1))
    case Positive(t1) => positive(this(t1))
    case Add(t1, t2) => add(this(t1), this(t2))
  }
}
