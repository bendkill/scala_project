import sast._
import tokens._

object Parser {
  def err (msg: String) : Option[(Term, List[Token])] = {
    println(s"parse error: $msg")
    None
  }

  def postFix (t1: Term, toks: List[Token]) : Option[(Term, List[Token])] =
    toks match {
      case Plus :: toks => nextTerm(toks) match {
        case Some((t2, toks)) => Some((Add (t1, t2), toks))
        case None => err("hanging +")
      }
      case Minus :: toks => nextTerm(toks) match {
        case Some((t2, toks)) => Some (Subtract (t1, t2), toks)
        case None => err("hanging -")
      }
      case _ => Some((t1, toks))
    }

  def nextSubTerm (toks : List[Token]) : Option[(Term, List[Token])] = toks match {
    case Dot :: Digits(s) :: toks =>
      Some((Decimal(false, 0, s.toInt), toks))
    case Digits(b) :: Dot :: Digits(d) :: toks =>
      Some((Decimal(false, b.toInt, d.toInt), toks))
    case Digits(b) :: Dot :: toks =>
      Some((Decimal(false, b.toInt, 0), toks))
    case Digits(s) :: toks => Some((Natural(s.toInt), toks))
    case LParen :: toks => nextTerm(toks) match {
      case Some((term, RParen :: toks)) => Some((term, toks))
      case Some((term, _)) => err("hanging (")
      case None => err("hanging (")
    }
    case _ => err("expected term")
  }

  def nextTerm (toks : List[Token]) : Option[(Term, List[Token])] = toks match {
    case Minus :: toks => nextSubTerm(toks) match {
      case Some((term, toks)) => postFix(Negate(term), toks)
      case None => None
    }
    case Plus :: toks => nextSubTerm(toks) match {
      case Some((term, toks)) => postFix(Positive(term), toks)
      case None => None
    }
    case toks => nextSubTerm(toks) match {
      case Some((term, toks)) => postFix(term, toks)
      case None => None
    }
  }

  def apply (toks : List[Token]) : Term = nextTerm(toks) match {
    case Some((term, Nil)) => term
    case Some((term, _)) => println("too many toks"); Empty
    case None => Empty
  }
}
