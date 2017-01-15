import java.util.Scanner

object LCS
{
  def lcs(l: Array[Int], r: Array[Int]): Int =
  {
    val a = Array.ofDim[Int](l.length+1, r.length+1)

    a(0)(0) = 0
    for (i <- 1 to l.length) a(i)(0) = 0
    for (j <- 1 to r.length) a(0)(j) = 0

    for
    {
      i <- 1 to l.length
      j <- 1 to r.length
    }
    {
      a(i)(j) = List( a(i-1)(j), a(i)(j-1), a(i-1)(j-1) + (if (l(i-1) == r(j-1)) 1 else 0)).max
    }

    a(l.length)(r.length)
  }

  def lcs3(l: Array[Int], r: Array[Int], t: Array[Int]): Int = // left, right, top
  {
    val a = Array.ofDim[Int](l.length+1, r.length+1, t.length+1)

    a(0)(0)(0) = 0
    for (i <- 1 to l.length) a(i)(0)(0) = 0
    for (j <- 1 to r.length) a(0)(j)(0) = 0
    for (k <- 1 to t.length) a(0)(0)(k) = 0

    val dijk = for
    {
      di <- List(-1,0)
      dj <- List(-1,0)
      dk <- List(-1,0)
      if (!(di == 0 && dj == 0 && dk == 0)  && !(di != 0 && dj != 0 && dk != 0)) // not (0,0,0) and not (-1,-1,-1)
    } yield (di,dj,dk)


    for
    {
      i <- 1 to l.length
      j <- 1 to r.length
      k <- 1 to t.length
    }
    {
      val prev = dijk.map(d => a(i+d._1)(j+d._2)(k+d._3))
      a(i)(j)(k) = Math.max( prev.max,  a(i-1)(j-1)(k-1) + (if (l(i-1) == r(j-1) && r(j-1) == t(k-1)) 1 else 0))
    }

    a(l.length)(r.length)(t.length)
  }

  def main(args: Array[String]) =
  {
    val s = new Scanner(System.in)

    val a = (for (i <- 1 to s.nextInt) yield s.nextInt).toArray
    val b = (for (i <- 1 to s.nextInt) yield s.nextInt).toArray
    val c = (for (i <- 1 to s.nextInt) yield s.nextInt).toArray
    val out = lcs3(a,b,c)
    println(out)
  }
}
