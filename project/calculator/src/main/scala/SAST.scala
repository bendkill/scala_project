package sast

trait Term {
  def tos () : String;
  override def toString () = tos
}

object Empty extends Term {
  def tos () = "Empty" // special term for when no terms exist
}

case class Natural(n:Int) extends Term {
  assert(n >= 0)
  def tos () = s"Natural($n)"
}

case class Decimal(isNeg:Boolean, base:Int, dec:Int) extends Term{
  assert(base >= 0)
  assert(dec >= 0)
  def tos () = s"Decimal(" ++ (if (isNeg) "-" else "") ++ s"$base.$dec)"
}

case class Negate(t1 : Term) extends Term {
  def tos () = s"Negate($t1)"
}

case class Positive(t1 : Term) extends Term {
  def tos () = s"Positive($t1)"
}

case class Add(a : Term, b : Term) extends Term {
  def tos () = s"Add($a, $b)"
}

case class Subtract(a : Term, b : Term) extends Term {
  def tos () = s"Subtract($a, $b)"
}


