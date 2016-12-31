import java.util.Scanner

object Majority
{
  def majority(a: Array[Int]): Boolean =
  {

    false
  }
  def main(args: Array[String]): Unit =
  {
    val s = new Scanner(System.in)

    val n = s.nextInt
    val a = new Array[Int](n)
    for (i <- 0 to n-1) a(i) = s.nextInt

    println(if (majority(a)) 1 else 0)
  }

}
