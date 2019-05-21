import tokens._

object Scanner {
  def nextToken (cs: List[Char]) : Option[(Token, List[Char])] = cs match {
    case (' ' | '\t') :: cs => nextToken(cs)
    case '(' :: cs => Some(LParen, cs)
    case ')' :: cs => Some(RParen, cs)
    case '-' :: cs => Some(Minus, cs)
    case '+' :: cs => Some(Plus, cs)
    case '.' :: cs => Some(Dot, cs)
    case c :: cs =>
      if (c.isDigit) {
        val (xs, rs) = (c::cs).span(x => x.isDigit)
        Some (Digits(xs.mkString), rs)
      } else None
    case _ => None
  }

  def apply (s:String) : List[Token] = {
    def lp (cs : List[Char]) : List[Token] = cs match {
      case Nil => Nil
      case cs => nextToken(cs) match {
        case Some((tok, cs)) => tok :: lp(cs)
        case None => Nil
      }
    }
    lp(s.toList)
  }
}
