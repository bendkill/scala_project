package nat

trait Nat {
  def isZero: Boolean;
  def pred: Nat;
  def succ: Nat;
  def + (y: Nat): Nat = if (y.isZero) this else this.succ + y.pred
  def - (y: Nat): Nat = if (y.isZero) this else this.pred + y.pred
  def * (y: Nat): Nat = if (y.isZero) y else this + this * y.pred
  def ^ (y: Nat): Nat = if (y.isZero) y.succ else this * (this ^ y.pred)
  def == (y: Nat): Boolean = {
    if (this.isZero) y.isZero
    else if (y.isZero) false
    else this.pred == y.pred
  }
  def != (y: Nat): Boolean = !(this == y)
  def > (y: Nat): Boolean = (this.isZero, y.isZero) match {
    case (true, _) => false
    case (false, true) => true
    case (false, false) => this.pred > y.pred
  }
  def >= (y: Nat): Boolean = (this.isZero, y.isZero) match {
    case (_, true) => true
    case (true, false) => false
    case (false, false) => this.pred >= y.pred
  }
  def <= (y: Nat): Boolean = !(this > y)
  def < (y: Nat): Boolean = !(this >= y)
}

object Zero extends Nat {
  def isZero = true
  def succ = Succ(this)
  def pred = this
  override def toString () = "Z"
}

case class Succ(v: Nat) extends Nat {
  def isZero = false
  def succ = Succ(this)
  def pred = v
  override def toString () = s"S($v)"
}
