package nat

import ord._

trait Nat {
  def isZero: Boolean;
  def pred: Nat;
  def succ: Nat;
  def + (that: Nat): Nat = if (that.isZero) this else this.succ + that.pred
  def - (that: Nat): Nat = if (that.isZero) this else this.pred - that.pred
  def * (that: Nat): Nat = if (that.isZero) that else this + this * that.pred
  def ^ (that: Nat): Nat = if (that.isZero) that.succ else this * (this ^ that.pred)
  def <= (that: Nat): Boolean = NatOrdering.leq(this, that)
  def >= (that: Nat): Boolean = NatOrdering.geq(this, that)
  def <  (that: Nat): Boolean = NatOrdering.lt(this, that)
  def >  (that: Nat): Boolean = NatOrdering.gt(this, that)
  def == (that: Nat): Boolean = NatOrdering.eq(this, that)
  def != (that: Nat): Boolean = NatOrdering.neq(this, that)
  def isEven: Boolean = if (this.isZero) true else !this.pred.isEven
  def isOdd: Boolean = !this.isEven
}

object NatOrdering extends Ordering[Nat] {
  def leq (x: Nat, y: Nat): Boolean =
    (x.isZero, y.isZero) match {
      case (true, _) => true
      case (false, true) => false
      case (false, false) => leq(x.pred, y.pred)
    }
}

object Zero extends Nat {
  override def toString () = "Z"
  def isZero = true
  def succ = Succ(this)
  def pred = this
}

case class Succ(n: Nat) extends Nat{
  override def toString () = s"S($n)"
  def isZero = false
  def succ = Succ(this)
  def pred = n
}
