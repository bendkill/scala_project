object Zero extends Nat {
  def isZero = true
  def succ = Succ(this)
  def pred = this
}
