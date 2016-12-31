import java.util.Scanner

object Lottery
{
  case class Segment(left: Int, right: Int)

  class Segments(segs: Array[Segment])
  {
    def countContaining(x: Int): Int =
    {
      0
    }
  }

  def main(args: Array[String]): Unit =
  {
    val s = new Scanner(System.in)

    val segCount = s.nextInt
    val pointCount = s.nextInt

    val segs = new Array[Segment](segCount)
    val points = new Array[Int](pointCount)

    for (i <- 0 to segCount-1) segs(i) = Segment(s.nextInt, s.nextInt)

    val segsIndex = new Segments(segs)
    for (i <- 0 to pointCount) print(segsIndex.countContaining(s.nextInt) + " ")
    println
  }

}
