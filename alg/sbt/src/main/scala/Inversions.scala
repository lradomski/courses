import java.util.Scanner

object Inversions
{
  def inversions(a: Array[Int]): Int =
  {
    a.length
  }

  def main(args: Array[String]): Unit =
  {
    val s = new Scanner(System.in)

    val n = s.nextInt
    val a = new Array[Int](n)
    for (i <- 0 to n-1) a(i) = s.nextInt

    println(inversions(a))
  }
}
