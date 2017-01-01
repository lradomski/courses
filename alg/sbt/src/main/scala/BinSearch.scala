import java.util.Scanner

import scala.annotation.tailrec

object BinSearch
{
  def find(a: Array[Int], key: Int): Int =
  {
    @tailrec
    def core(start: Int, end: Int): Int =
    {
      if (end < start) start - 1
      else
      {
        val mid = start + (end - start) / 2
        val test = a(mid)
        if (test == key) mid
        else if (key < test) core(start, mid - 1)
        else if (test > key) core(mid + 1, end)
        else -1
      }
    }

    val out = core(0, a.length - 1)
    if (0 <= out && out < a.length && a(out) == key) out else -1
  }

  def main(args: Array[String]): Unit =
  {
    val s = new Scanner(System.in)

    val n = s.nextInt
    val a = new Array[Int](n)
    for (i <- 0 to n - 1) a(i) = s.nextInt

    val k = s.nextInt
    for (i <- 0 to k - 1) print(find(a, s.nextInt) + " ")
    println
  }

}
