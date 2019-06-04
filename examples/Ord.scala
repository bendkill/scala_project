package ord

trait Ordering[T] {
  def leq (x: T, y: T): Boolean; // abstract method
  def geq (x: T, y: T): Boolean =  leq(y, x)
  def gt  (x: T, y: T): Boolean = !leq(x, y)
  def lt  (x: T, y: T): Boolean = !leq(y, x)
  def eq  (x: T, y: T): Boolean =  leq(x, y) &&  leq(y, x)
  def neq (x: T, y: T): Boolean = !leq(x, y) || !leq(y, x)
}
