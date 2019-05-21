object Main extends App {
  println("Program execution start.")
  while (true) {
    val line = scala.io.StdIn.readLine("> ")
    if (line == "q")
      // TODO: exit gracefully
      System.exit(0)
    val toks = Scanner(line)
    val term = Parser(toks)
    println(term)
  }
  println("Program execution end.")
}
