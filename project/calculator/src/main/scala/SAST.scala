package sast

trait Term {
  def tos () : String;
  override def toString () = tos
}



