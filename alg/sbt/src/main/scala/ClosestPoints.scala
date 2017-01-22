import java.text.DecimalFormat
import java.util.Scanner

import scala.annotation.tailrec
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
      @tailrec
      def find(x: Int, l: Int, r: Int, sameOnRight: Boolean): Int =
      {
        if (l >= r) l
        else
        {
//          assert(points(l).x <= x)
//          assert(x <= points(r).x)

          val m = (l + r) / 2
          if (if (sameOnRight) x <= points(m).x else x < points(m).x) find(x, l, m, sameOnRight)
          else find(x, m + 1, r, sameOnRight)
        }
      }

      def findRightOf(x: Int, l: Int, r: Int): Int =
      {
        val i = find(x, l, r, true)

        if (l < i) assert(points(i - 1).x < x, "findRightOf: " + points(i - 1).x + " not < " + x)
        i
      }

      def findLeftOf(x: Int, l: Int, r: Int): Int =
      {
        val i = find(x, l, r, false)
        if (i < r) assert(x < points(i + 1).x, "findLeftOf: " + x + " not < " + points(i + 1).x)
        i
      }

      val width = r - l + 1
      assert(width > 1, "width==1 ?")
      if (width == 2) distIdx(l, r)
      else if (width == 3) Seq(distIdx(l, l + 1), distIdx(l + 1, l + 2), distIdx(l, l + 2)).min
      else
      {
        val m = (l + r) / 2
        val ld = core(l, m)
        val rd = core(m + 1, r)
        val d = Math.min(ld, rd)
        val di = Math.ceil(d).toInt

        val mx = points(m).x

        val il = findRightOf(mx - di, l, m)
        val jr = findLeftOf(mx + di, m + 1, r)
        val ds = for
        {
          i <- il to m
          pl = points(i)
          if (pl.x >= mx - d)
          j <- m + 1 to jr
          pr = points(j)
          if (pr.x <= mx + d)
          if (Math.abs(pl.y - pr.y) <= d)
        } yield dist(pl, pr)

        (d +: ds).min
        //Math.min(d, ds.min)
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
    val uniquePoints = Set(rawPoints: _*) //(Ordering.fromLessThan( (l,r) => if (l.x < r.x) true else l.y < r.y))
    val points = uniquePoints.toArray.sortBy(_.x)
    //SortedSet(rawPoints: _*)(Ordering[Int].on[Point](_.x)).toArray // unique only


    if (points.length < n) println("0.0") // repeated point(s)
    else
    {
      val min = minDistance(points)
      println(formatter.format(min))
    }


  }
}
