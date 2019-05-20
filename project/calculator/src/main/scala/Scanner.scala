object Scanner {
  def expression (cs: List[Char])  {
    cs
  }
  def apply (s:String) {
    println("scanning " ++ s)
    println(expression(s.toList))
  }
}
