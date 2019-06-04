// Scala:
object PrintOptions {
  def main(args: Array[String]): unit = {
    println("Options selected:")
    for (val arg <- args)
      if (arg.startsWith("-"))
        println(" " + arg.substring(1))
  }
}
