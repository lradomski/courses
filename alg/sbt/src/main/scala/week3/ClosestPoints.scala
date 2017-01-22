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
      def findCore(points: Array[Point], get: Point => Int, v: Int, l: Int, r: Int, sameOnRight: Boolean): Int =
      {
        if (l >= r) l
        else
        {
          val m = (l + r) / 2
          if (if (sameOnRight) v <= get(points(m)) else v < get(points(m))) findCore(points, get, v, l, m, sameOnRight)
          else findCore(points, get, v, m + 1, r, sameOnRight)
        }
      }

      def find(x: Int, l: Int, r: Int, sameOnRight: Boolean): Int =
      {
        findCore(points, _.x, x, l, r, sameOnRight)
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

      def findUpOf(points: Array[Point], y: Int, l: Int, r: Int): Int =
      {
        val i = findCore(points, _.y, y, l, r, true)

        if (l < i) assert(points(i - 1).y < y, "findUpOf: " + points(i - 1).y + " not < " + y)
        i
      }

      def findDownOf(points: Array[Point], y: Int, l: Int, r: Int): Int =
      {
        val i = findCore(points, _.y, y, l, r, false)
        if (i < r) assert(y < points(i + 1).y, "findDownOf: " + y + " not < " + points(i + 1).y)
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

        // all "left" points within d of mx
        val lps = for {
          i <- il to m
          pl = points(i)
          if (pl.x >= mx - d)
        } yield pl

        // all "right" points within d of mx
        val rps = for
        {
          j <- m + 1 to jr
          pr = points(j)
          if (pr.x <= mx + d)
        } yield pr

        if (rps.isEmpty) d
        else
        {
          // "right" candidate points sorted by y
          val rpsy = rps.toArray.sortBy(p => p.y)

          // for each "left" candidate find range of points among "right" candidates with (d,2d) rectangle
          // since "right" points are sorted by y we can find index range by binary search (findUpOf/findDownOf)
          // we compute distances of lef-right pairs of points this way (left points with right points in their rectangle if any)
          // (there can be at most 6 points in left point's rectangle see: https://www.cs.ucsb.edu/~suri/cs235/ClosestPair.pdf )
          val ds = for
          {
            pl <- lps
            j <- findUpOf(rpsy, pl.y - di, 0, rpsy.length - 1) to findDownOf(rpsy, pl.y + di, 0, rpsy.length - 1)
            pr = rpsy(j)
            //if (Math.abs(pl.y - pr.y) <= d)
          } yield dist(pl, pr)

          //        val ds = for
          //          {
          //            pl <- lps
          //            pr <- rps
          //            if (Math.abs(pl.y - pr.y) <= d)
          //          } yield dist(pl,pr)


          //if (ds.isEmpty) d else Math.min(d, ds.min)
          (d +: ds).min
        }
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


    val rawPoints = (for (i <- 0 to n - 1) yield Point(s.nextInt, s.nextInt))
    val uniquePoints = Set(rawPoints: _*)
    val points = uniquePoints.toArray.sortBy(_.x)

    if (points.length < n) println("0.0") // repeated point(s)
    else
    {
      val min = minDistance(points)
      println(formatter.format(min))
    }


  }
}
