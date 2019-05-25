package ast

trait Term {
  var tau : ty.Ty = ty.Empty
  def withType (tau1: ty.Ty) = {tau = tau1; this}
  def isNat () : Boolean = false;
  def isVal () : Boolean = false;
}

object Empty extends Term {
  override def toString () = "Empty"
  override def isVal () = true
}

object True extends Term {
  override def toString () = "T"
  override def isVal () = true
}

object False extends Term {
  override def toString () = "F"
  override def isVal () = true
}

object Zero extends Term {
  override def toString () = "Z"
  override def isNat () = true
  override def isVal () = true
}

case class Succ(t1: Term) extends Term {
  override def toString () = s"S($t1)"
  override def isNat () = t1.isNat()
  override def isVal () = t1.isNat()
}

case class Pred(t1: Term) extends Term {
  override def toString () = s"P($t1)"
}

case class Integer(t1: Term, t2: Term) extends Term {
  // t1 : Bool, t2 natural
  override def toString () = s"I($t1, $t2)"
  override def isVal () = ((t1 == True) || (t1 == False)) && t2.isVal()
}

case class Rational(t1 : Term, t2: Term) extends Term {
  // t1: integer, t2: natural
  override def toString () = s"R($t1, $t2)"
  override def isVal () = t1.isVal() && t2.isVal() // improper rationals still values
}

case class Negate(t1: Term) extends Term {
  override def toString () = s"Neg($t1)"
}

case class Positive(t1: Term) extends Term {
  override def toString () = s"Pos($t1)"
}

case class Add(t1: Term, t2: Term) extends Term {
  override def toString () = s"Add($t1, $t2)"
}

case class Multiply(t1: Term, t2: Term) extends Term {
  override def toString () = s"Multiply($t1, $t2)"
}

case class Divide(t1: Term, t2: Term) extends Term {
  override def toString () = s"Divide($t1, $t2)"
}
