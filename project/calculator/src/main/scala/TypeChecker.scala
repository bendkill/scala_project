import ast._

object TypeChecker {
  // todo: more elegantly handle type error messages?
  def err (t: Term) : ty.Ty = {println(s"type error: $t"); ty.Error}

  def apply (t: Term) : ty.Ty = {
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
      case Add(t1, t2) => (this(t1), this(t2)) match {
        case (ty.Natural, tau2) => if (tau2.isNumeric) tau2 else err(t)
        case (ty.Integer, ty.Natural | ty.Integer) => ty.Integer
        case (ty.Rational, tau2) => if (tau2.isNumeric) ty.Rational else err(t2)
        case _ => err(t)
      }
      case Subtract(t1, t2) => (this(t1), this(t2)) match {
        case (ty.Natural | ty.Integer, ty.Natural | ty.Integer) => ty.Integer
        case (ty.Rational, tau2) => if (tau2.isNumeric) ty.Rational else err(t2)
        case _ => err(t)
      }
    }
    t.tau = tau
    tau
  }
}
