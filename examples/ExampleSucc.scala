case class Succ(v: Nat) extends Nat {
  def isZero = false
  def succ = Succ(this)
  def pred = v
}
