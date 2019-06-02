package olist

trait Ordered[+T] {
  def <= [U >: T](x: U): Boolean;
}

// trait Index {
//   def isZero: Boolean;
//   def pred: Index;
// }

trait OrderedList[+T <: Ordered[T]] {
  def isEmpty: Boolean;
  def insert[U >: T <: Ordered[U]](x: U): OrderedList[U];
}

object Emp extends OrderedList[Nothing] {
  override def toString () = "Emp"
  def isEmpty = true
  def insert[T <: Ordered[T]](x: T) = Cons(x, this)
}

case class Cons[+T <: Ordered[T]] (head: T, tail: OrderedList[T])
    extends OrderedList[T] {
  override def toString () = s"($head :: $tail)"
  def isEmpty = false
  // [U >: T <: Ordered[T]]
  def insert[U >: T <: Ordered[U]](x: U) =
    if (x <= head) Cons(x, this) else Cons(head, tail.insert(x))
  // def apply[T <: Nat](i: Int) : T =
  //   if (i == 0) head else tail.apply(i - 1)
}

