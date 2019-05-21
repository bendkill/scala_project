package sast

trait Term {
  def tos () : String;
  override def toString () = tos
}

object Empty extends Term {
  def tos () = "" // special term for when no terms exist
}

case class Natural(n:Int) extends Term {
  assert(n >= 0)
  def tos () = s"Natural($n)"
}

case class Integer(isNeg:Boolean, n:Int) extends Term {
  assert(n>=0)
  def tos () = s"Integer(" ++ (if (isNeg) "-" else "") ++ s"$n)"
}

case class Decimal(isNeg:Boolean, base:Int, dec:Int) extends Term{
  assert(base >= 0)
  assert(dec >= 0)
  def tos () = s"Decimal(" ++ (if (isNeg) "-" else "") ++ s"$base.$dec)"
}

case class Add(a : Term, b : Term) extends Term {
  def tos () = s"Add($a, $b)"
}

case class Subtract(a : Term, b : Term) extends Term {
  def tos () = s"Subtract($a, $b)"
}


