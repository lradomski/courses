import java.util.Scanner

object QuickSort3Way
{
  def qsort3way(a: Array[Int]): Array[Int] =
  {
    a
  }

  def main(args: Array[String]): Unit =
  {
    val s = new Scanner(System.in)

    val n = s.nextInt
    val a = new Array[Int](n)
    for (i <- 0 to n-1) a(i) = s.nextInt

    val k = s.nextInt
    val as = qsort3way(a)
    as.foreach(e => print(e + " "))
    println
  }

}
