import java.util.Scanner


object FiboSmall {
  def main(args: Array[String]) = {
    val s = new Scanner(System.in);

    val n = s.nextInt();
    println( fibonacci(n) );
  }

  def fibonacci(n: Int): Int = {
    require(n >= 0, "n >= 0")
    require(n <= 45, "n <= 45")

    val f = new Array[Int](3)

    for (i <- 0 to n) {
      if (i <= 1) f(i % 3) = i
      else {
        f(i % 3) = f((i - 1) % 3) + f((i - 2) % 3)
      }
    }

    f(n % 3)
  }

}
