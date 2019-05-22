package ast

trait Term {
  def tos () : String;
  override def toString () = tos
  var tau : ty.Ty = ty.Empty
  def withType (tau1: ty.Ty) = {tau = tau1; this}
}

object Empty extends Term {
  def tos () = "Empty"
}

object True extends Term {
  def tos () = "T"
}

object False extends Term {
  def tos () = "F"
}

object Zero extends Term {
  def tos () = "Z"
}

case class Succ(t1: Term) extends Term {
  def tos () = s"S($t1)"
}

case class Pred(t1: Term) extends Term {
  def tos () = s"P($t1)"
}

case class Integer(t1: Term, t2: Term) extends Term {
  // t1 : Bool, t2 natural
  def tos () = s"I($t1, $t2)"
}

case class Rational(t1 : Term, t2: Term) extends Term {
  // t1: integer, t2: natural
  def tos () = s"R($t1, $t2)"
}

case class Negate(t1: Term) extends Term {
  def tos () = s"Neg($t1)"
}

case class Positive(t1: Term) extends Term {
  def tos () = s"Pos($t1)"
}

case class Add(t1: Term, t2: Term) extends Term {
  def tos () = s"Add($t1, $t2)"
}

