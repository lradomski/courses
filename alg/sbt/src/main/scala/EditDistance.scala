import java.util.Scanner

object EditDistance
{
  def editDistance(s1: String, s2: String): Int =
  {
    val a = Array.ofDim[Int](s1.length+1, s2.length+1)

    a(0)(0) = 0
    for (i <- 1 to s1.length) a(i)(0) = i
    for (j <- 1 to s2.length) a(0)(j) = j

    for
      {
        i <- 1 to s1.length
        j <- 1 to s2.length
      }
      {
        a(i)(j) = List( a(i-1)(j)+1, a(i)(j-1)+1, a(i-1)(j-1) + (if (s1(i-1) != s2(j-1)) 1 else 0)).min
      }

    a(s1.length)(s2.length)
  }

  def main(args: Array[String]) =
  {
    val s = new Scanner(System.in)

    val s1 = s.next
    val s2 = s.next

    val out = editDistance(s1, s2)
    println(out)
  }
}
