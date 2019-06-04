// Java:
class PrintOptions {
  public static void main(String[] args) {
    System.out.println("Options selected:");
    for (int i = 0; i < args.length; i++)
      if (args[i].startsWith("-"))
        System.out.println(" " + args.substring(1))
  }
}
