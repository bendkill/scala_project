import nat._
import ord._
import equiv._

trait NatOrderingWithEquiv extends Ordering[Nat] with Equiv[Nat] {
  def leq (x: Nat, y: Nat): Boolean = NatOrdering.leq(x,y)
  def equal (x: Nat, y: Nat): Boolean = x.isEven == y.isEven
}
