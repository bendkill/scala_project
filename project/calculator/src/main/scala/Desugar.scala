object Desugarer {
  def nat (n: Int) : ast.Term = {
    assert(n >= 0)
    if (n == 0)
      ast.Zero
    else
      ast.Succ(nat(n-1))
  }
  def mag (n: Int) : Int = math.pow(10, math.ceil(math.log10(n))).toInt

  def bool (b:Boolean) : ast.Term = if (b) ast.True else ast.False

  def apply (st: sast.Term) : ast.Term = st match {
    case sast.Empty => ast.Empty
    case sast.Natural(n) => nat(n)
    case sast.Negate(t1) => ast.Negate(this(t1))
    case sast.Positive(t1) => ast.Positive(this(t1))
    case sast.Decimal(isNeg, base, dec) =>
      ast.Add(
        ast.Rational(ast.Integer(bool(isNeg), nat(base)), ast.Succ(ast.Zero)),
        ast.Rational(ast.Integer(ast.False, nat(dec)), nat(mag(dec)))
      )
    case sast.Add(a,b) => ast.Add(this(a), this(b))
    case sast.Subtract(a,b) => ast.Add(this(a), ast.Negate(this(b)))
  }
}
