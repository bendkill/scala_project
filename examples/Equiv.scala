package equiv

trait Equiv[T] {
  def eq  (x: T, y: T): Boolean; // abstract
  def neq (x: T, y: T): Boolean = !neq(x, y)
}
