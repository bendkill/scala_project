package sast

trait Term {}

object Empty extends Term {
  override def toString () = "Empty" // special term for when no terms exist
}

case class Natural(n:String) extends Term {
  override def toString () = s"Natural($n)"
}

case class Decimal(isNeg:Boolean, base:String, dec:String) extends Term{
  override def toString () = s"Decimal(" ++ (if (isNeg) "-" else "") ++ base ++ "." ++ dec ++ ")"
}

case class Negate(t1 : Term) extends Term {
  override def toString () = s"Negate($t1)"
}

case class Positive(t1 : Term) extends Term {
  override def toString () = s"Positive($t1)"
}

case class Add(a : Term, b : Term) extends Term {
  override def toString () = s"Add($a, $b)"
}

case class Subtract(a : Term, b : Term) extends Term {
  override def toString () = s"Subtract($a, $b)"
}

case class Multiply(a: Term, b: Term) extends Term {
  override def toString () = s"Multiply($a, $b)"
}

case class Divide(a: Term, b: Term) extends Term {
  override def toString () = s"Divide($a, $b)"
}
