package Set

trait Set[T]{
  def include(x: T): Set[T]
  def contains(x: T): boolean
}

implicit def listToSet[T](xs: GenList[T]): Set[T] =
  new Set[T] {
    def include(x: T): Set[T] = 
      xs prepend x
    def contains (x: T): boolean = 
      !isEmpty && (xs.head == x || (xs.tail contains x))
  }

/*****************************************************/
//Assume that xs is a value of type GenList[T]
val s: Set[T] = xs;
xs contains x

//Compiler inserts applications of the view =>

val s: Set[T] = listToSet(xs);
listToSet(xs) contains x;

