object Desugarer {
  def toNat (n: Int) : ast.Term = {
    if (n == 0)
      ast.Zero
    else
      ast.Succ(toNat(n-1))
  }

  def nat (s: String) : ast.Term = {
    toNat(s.toInt)
  }

  def bool (b:Boolean) : ast.Term = if (b) ast.True else ast.False

  def mag(d: String) : ast.Term = toNat(Math.pow(10, d.length()).toInt)

  def apply (st: sast.Term) : ast.Term = st match {
    case sast.Empty => ast.Empty
    case sast.Natural(n) => nat(n)
    case sast.Negate(t1) => ast.Negate(this(t1))
    case sast.Positive(t1) => ast.Positive(this(t1))
    case sast.Decimal(isNeg, base, dec) =>
      ast.Add(
        ast.Rational(ast.Integer(bool(isNeg), nat(base)), ast.Succ(ast.Zero)),
        ast.Rational(ast.Integer(ast.False, nat(dec)), mag(dec))
      )
    case sast.Add(a,b) => ast.Add(this(a), this(b))
    case sast.Subtract(a,b) => ast.Add(this(a), ast.Negate(this(b)))
    case sast.Multiply(a,b) => ast.Multiply(this(a), this(b))
    case sast.Divide(a,b) => ast.Divide(this(a), this(b))
  }
}
