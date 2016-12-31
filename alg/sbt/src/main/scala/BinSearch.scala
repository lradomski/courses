import java.util.Scanner

object BinSearch
{
  def find(array: Array[Int], i: Int): Int =
  {
    -1
  }

  def main(args: Array[String]): Unit =
  {
    val s = new Scanner(System.in)

    val n = s.nextInt
    val a = new Array[Int](n)
    for (i <- 0 to n-1) a(i) = s.nextInt

    val k = s.nextInt
    for (i <- 0 to k-1) print(find(a,s.nextInt) + " ")
    println
  }

}
