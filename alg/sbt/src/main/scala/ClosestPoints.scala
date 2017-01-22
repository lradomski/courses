import java.text.DecimalFormat
import java.util.Scanner

import scala.collection.SortedSet
import scala.collection.immutable.TreeSet


object ClosestPoints
{

  case class Point(x: Int, y: Int)

  def pow2(x: Double) = x * x

  def dist(l: Point, r: Point) = Math.sqrt(pow2(l.x - r.x) + pow2(l.y - r.y))

  def minDistance(points: Array[Point]): Double =
  {
    def distIdx(l: Int, r: Int) = dist(points(l), points(r))

    def core(l: Int, r: Int): Double =
    {
      val width = r - l
      assert(width > 1, "width==1 ?")
      if (width == 2) distIdx(l, r)
      else if (width == 3) Seq(distIdx(l, l + 1), distIdx(l + 1, l + 2), distIdx(l, l + 2)).min
      else
      {
        val m = (l + r) / 2
        val ld = core(l, m)
        val rd = core(m + 1, r)
        val d = Math.min(ld, rd)

        val mx = points(m).x

        // find in L p.x >= mx - d  -> LD
        // find points in P in rectangle = RECT
        // for each LD and RECT calc dist = D2
        // return min(d, D2.min)

        0.0



      }


    }

    if (points.length < 2) 0.0
    else core(0, points.length - 1)
  }


  def main(args: Array[String]): Unit =
  {
    val s = new Scanner(System.in)
    val formatter = new DecimalFormat("#.####")

    val n = s.nextInt


    val rawPoints = (for (i <- 0 to n - 1) yield Point(s.nextInt, s.nextInt)) //.toSet(Ordering[Int].on[Point](_.x))
    val points = SortedSet(rawPoints: _*)(Ordering[Int].on[Point](_.x)).toArray // unique only


    if (points.length < n) println("0.0") // repeated point(s)
    else
    {
      val min = minDistance(points)
      println(formatter.format(min))
    }


  }
}
