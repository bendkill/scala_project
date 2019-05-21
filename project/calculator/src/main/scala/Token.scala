package tokens

trait Token {
  def tos : String;
  override def toString () : String = tos
}

object LParen extends Token {
  def tos () = "("
}

object RParen extends Token {
  def tos () = ")"
}

object Minus extends Token {
  def tos () = "-"
}

object Plus extends Token {
  def tos () = "+"
}

object Dot extends Token {
  def tos () = "."
}

case class Digits(x: String) extends Token {
  def tos () = x
}


