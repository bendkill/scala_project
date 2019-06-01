package nats

trait Nat {
  def isZero (): Boolean
  def pred (): Nat
  def succ (): Nat
  def + (y: Nat): Nat = if (y.isZero) this else this.succ + y.pred
  def - (y: Nat): Nat = if (y.isZero) this else this.pred + y.pred
}

object Z extends Nat {
  def isZero () = true
  def succ () = Succ(this)
  def pred () = this
}

case class Succ(v: Nat) extends Nat {
  def isZero () = false
  def succ () = Succ(this)
  def pred () = v
}
