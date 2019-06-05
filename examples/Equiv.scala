package equiv

trait Equiv[T] {
  def equal  (x: T, y: T): Boolean;
  def neq (x: T, y: T): Boolean = !equal(x, y)
}
