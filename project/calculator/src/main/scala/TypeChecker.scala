import ast._

object TypeChecker {
  // todo: more elegantly handle type error messages?
  def err (t: Term) : ty.Ty = {println(s"type error: $t"); ty.Error}

  def atLeastInteger(tau: ty.Ty) : ty.Ty = tau match {
    case ty.Natural => ty.Integer
    case ty.Integer => ty.Integer
    case ty.Rational => ty.Rational
    case _ => err(Empty)
  }

  def atLeastRational(tau: ty.Ty) : ty.Ty = tau match {
    case ty.Natural | ty.Integer | ty.Rational => ty.Rational
    case _ => err(Empty)
  }

  def combine(tau1: ty.Ty, tau2: ty.Ty) : ty.Ty = (tau1, tau2) match {
    case (ty.Natural, tau2) => if (tau2.isNumeric) tau2 else err(Empty)
    case (ty.Integer, ty.Natural | ty.Integer) => ty.Integer
    case (ty.Rational, tau2) => if (tau2.isNumeric) ty.Rational else err(Empty)
    case _ => err(Empty)
  }

  def apply (t: Term) : ty.Ty = {
    var ctau : ty.Ty = ty.Empty
    val tau = t match {
      case Empty => ty.Empty
      case True | False => ty.Bool
      case Zero => ty.Natural
      case Succ(t1) => if (this(t1) == ty.Natural) ty.Natural else err(t)
      case Pred(t1) => if (this(t1) == ty.Natural) ty.Natural else err(t)
      case Integer(t1, t2) =>
        if (this(t1) == ty.Bool && this(t2) == ty.Natural) ty.Integer else err(t)
      case Rational(t1, t2) =>
        if (this(t1) == ty.Integer && this(t2) == ty.Natural) ty.Rational else err(t)
      case Negate(t1) => atLeastInteger(this(t1))
      case Positive(t1) => atLeastInteger(this(t1))
      case Add(t1, t2) => combine(this(t1), this(t2))
      case Multiply(t1, t2) => combine(this(t1), this(t2))
      case Divide(t1, t2) =>
        ctau = combine(this(t1), this(t2))
        ty.Rational
    }
    t.tau = if (ctau == ty.Empty) tau else ctau
    tau
  }
}
