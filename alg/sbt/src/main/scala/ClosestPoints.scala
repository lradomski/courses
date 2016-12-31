import java.text.DecimalFormat
import java.util.Scanner



object ClosestPoints
{
  case class Point(x: Int, y: Int)

  def minDistance(points: Array[Point]): Double =
  {
    0.0
  }


  def main(args: Array[String]): Unit =
  {
    val s = new Scanner(System.in)
    val formatter = new DecimalFormat("#.####")


    val n = s.nextInt

    val points = new Array[Point](n)

    for (i <- 0 to n-1) points(i) = Point(s.nextInt, s.nextInt)

    val min = minDistance(points)
    println(formatter.format(min))
  }
}
