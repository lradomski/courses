import java.util.Scanner


import scala.annotation.tailrec

object Segments
{

  case class Seg(start: Int, end: Int)
  {
    def covers(point: Int): Boolean = start <= point && point <= end
  }

  @tailrec
  def coverPoints(segs: List[Seg], points: List[Int]): List[Int] =
  {
    if (segs.isEmpty) points
    else
    {
      val seg = segs.head
      val point = seg.end
      val newSegs = segs.filter(s => !s.covers(point))
      coverPoints(newSegs, points ::: List[Int](point))
    }
  }

  def main(args: Array[String]): Unit =
  {
    val s = new Scanner(System.in);

    def sort(l: Seg, r: Seg) = l.end < r.end

    @tailrec
    def readSegments(n: Int, segs: List[Seg]): List[Seg] =
    {
      if (n > 0) readSegments(n - 1, Seg(s.nextInt, s.nextInt) :: segs)
      else segs
    }

    val n = s.nextInt
    val segs = readSegments(n, List[Seg]()).sortWith(sort)
    val points = coverPoints(segs, List[Int]())
    println(points.length)
    points.foreach(point => print(point + " "))
    println
  }

}
