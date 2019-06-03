package ordlist
import ord._

trait OrdList[+T] {
  def isEmpty: Boolean;
  def insert[U >: T](x: U, o: Ordering[U]): OrdList[U];
}

object Emp extends OrdList[Nothing] {
  override def toString = "Emp"
  def isEmpty = true
  def insert[U >: Nothing](x: U, o: Ordering[U]) =
    Cons(x, this.asInstanceOf[OrdList[U]])
}

case class Cons[+T] (head: T, tail: OrdList[T]) extends OrdList[T] {
  override def toString = s"$head :: $tail"
  def isEmpty = false
  def insert[U >: T](x: U, o: Ordering[U]) =
    if (o.leq(x, head)) Cons(x, this) else Cons(head, tail.insert(x, o))
}
