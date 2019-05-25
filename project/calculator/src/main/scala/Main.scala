object Main extends App {
  println("Program execution start.")
  while (true) {
    val line = scala.io.StdIn.readLine("> ")
    if (line == "q")
      // TODO: exit gracefully
      System.exit(0)
    val toks = Scanner(line)
    println(toks)
    val sterm = Parser(toks)
    println(sterm)
    val term = Desugarer(sterm)
    println(term)
    val tau = TypeChecker(term)
    if (tau == ty.Error) {
      println("Empty : Error")
    } else {
      val value = Evaluator(term)
      println(s"$value : $tau")
      println(Sugarer(value))
    }
  }
  println("Program execution end.")
}
