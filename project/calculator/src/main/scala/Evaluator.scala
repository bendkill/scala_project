import ast._

object Evaluator {
  def castErr (tau: ty.Ty) : Term = {
    println(s"cast error: $tau")
    Empty
  }

  def err (t: Term) : Term = {
    println(s"eval error: $t")
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
    case Add(t1, t2) => t.tau match {
      case ty.Natural => cast(this(t1), t.tau) match {
        case Zero => cast(this(t2), t.tau)
        case Succ(v1) => this(Add(v1, Succ(t2)).withType(t.tau))
        case _ => err(t)
      }
      case ty.Integer => cast(this(t1), t.tau) match {
        case Integer(False | True, Zero) => cast(this(t2), t.tau)
        case Integer(False, Succ(v1)) => cast(this(t2), t.tau) match {
          case Integer(True | False, Zero) => Integer(False, Succ(v1))
          case Integer(False, v2) =>
            this(Add(Integer(False, v1), Integer(False, Succ(v1))).withType(t.tau))
          case Integer(True, Succ(v2)) =>
            this(Add(Integer(False, v1), Integer(True, v2)).withType(t.tau))
        }
        case Integer(True, Succ(v1)) => cast(this(t2), t.tau) match {
          case Integer(True | False, Zero) => Integer(True, Succ(v1))
          case Integer(False, Succ(v2)) =>
            this(Add(Integer(True, v1), Integer(False, v2)).withType(t.tau))
          case Integer(True, v2) =>
            this(Add(Integer(True, v1), Integer(True, Succ(v2)).withType(t.tau)))
        }
      }
      case ty.Rational => err(t) // TODO
      case _ => err(t)
    }
    case Subtract(t1, t2) => Empty // TODO
  }
}
