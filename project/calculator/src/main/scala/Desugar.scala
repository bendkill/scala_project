object Desugarer {
  def nat (n: Int) : ast.Term = {
    if (n == 0)
      ast.Zero
    else
      ast.Succ(nat(n-1))
  }
  def mag (n: Int) : Int = math.ceil(math.log10(n)).toInt

  def bool (b:Boolean) : ast.Term = if (b) ast.True else ast.False

  def apply (st: sast.Term) : ast.Term = st match {
    case sast.Empty => ast.Empty
    case sast.Natural(n) => nat(n)
    case sast.Integer(isNeg, n) =>
      ast.Integer(bool(isNeg), nat(n))
    case sast.Decimal(isNeg, base, dec) =>
      ast.Add(
        ast.Rational(ast.Integer(bool(isNeg), nat(base)), ast.Succ(ast.Zero)),
        ast.Rational(ast.Integer(ast.False, nat(dec)), nat(mag(dec)))
      )
    case sast.Add(a,b) => ast.Add(this(a), this(b))
    case sast.Subtract(a,b) => ast.Subtract(this(a), this(b))
  }
}
