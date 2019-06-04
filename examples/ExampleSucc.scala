case class S(n: Nat) extends Nat {
  def isZero = false
  def succ = S(this)
  def pred = n
}
