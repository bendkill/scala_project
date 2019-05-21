import sast._
import tokens._

object Parser {
  def postFix (t1: Term, toks: List[Token]) : Option[(Term, List[Token])] =
    toks match {
      case Plus :: toks => nextTerm(toks) match {
        case Some((t2, toks)) => Some((Add (t1, t2), toks))
        case None => println("hanging +"); None
      }
      case Minus :: toks => nextTerm(toks) match {
        case Some((t2, toks)) => Some (Subtract (t1, t2), toks)
        case None => println("hanging -"); None
      }
      case _ => Some((t1, toks))
    }

  def nextTerm (toks : List[Token]) : Option[(Term, List[Token])] = toks match {
    case Minus :: Dot :: Digits(s) :: toks =>
      postFix(Decimal(true, 0, s.toInt), toks)
    case Dot :: Digits(s) :: toks =>
      postFix(Decimal(false, 0, s.toInt), toks)
    case Minus :: Digits(b) :: Dot :: Digits(d) :: toks =>
      postFix(Decimal(true, b.toInt, d.toInt), toks)
    case Digits(b) :: Dot :: Digits(d) :: toks =>
      postFix(Decimal(false, b.toInt, d.toInt), toks)
    case Minus :: Digits(b) :: Dot :: toks =>
      postFix(Decimal(true, b.toInt, 0), toks)
    case Digits(b) :: Dot :: toks =>
      postFix(Decimal(false, b.toInt, 0), toks)
    case Minus :: Digits(s) :: toks => postFix(Integer(true, s.toInt), toks)
    case Plus :: Digits(s) :: toks => postFix(Integer(false, s.toInt), toks)
    case Digits(s) :: toks => postFix(Natural(s.toInt), toks)
    case LParen :: toks => nextTerm(toks) match {
      case Some((term, RParen :: toks)) => postFix(term, toks)
      case Some((term, _)) => println("hanging ("); None
      case None => println("hanging ("); None
    }
    case _ => None
  }

  def apply (toks : List[Token]) : Term = nextTerm(toks) match {
    case Some((term, Nil)) => term
    case Some((term, _)) => println("too many toks"); Empty
    case None => Empty
  }
}
