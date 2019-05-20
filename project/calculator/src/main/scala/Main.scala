object Main extends App {
  println("Program execution start.")
  while (true) {
    val line = scala.io.StdIn.readLine("> ")
    if (line == "q")
      // TODO: exit gracefully
      System.exit(0)
    Scanner(line)
  }
  println("Program execution end.")
}
