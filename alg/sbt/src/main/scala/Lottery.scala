import java.util.Scanner

object Lottery
{

  trait Point
  {
    def x: Int

    def order: Int
  }

  case class Left(x: Int) extends Point
  {
    val order = 1
    override def toString = "+" + x + " "
  }

  case class Right(x: Int) extends Point
  {
    val order = 3
    override def toString = "-" + x + " "
  }

  case class Pick(x: Int, i: Int) extends Point
  {
    val order = 2
    override def toString = "*" + x + " "
  }

  def main(args: Array[String]): Unit =
  {
    val s = new Scanner(System.in)

    val segCount = s.nextInt
    val pickCount = s.nextInt

    var marks = new Array[Point](segCount * 2 + pickCount)
    //val segs = new Array[Segment](segCount)
    //val points = new Array[Int](pointCount)
    var m = 0

    for (i <- 0 until segCount)
    {
      marks(m) = Left(s.nextInt)
      marks(m + 1) = Right(s.nextInt)
      m += 2
    }

    for (i <- 0 until pickCount)
    {
      marks(m) = Pick(s.nextInt, i)
      m += 1
    }



    marks = marks.sortWith(
      (l, r) =>
      {
        if (l.x < r.x) true
        else if (l.x == r.x) l.order < r.order
        else false
      }
    )



    val picks = new Array[Int](pickCount)

    var count = 0
    for (i <- 0 until marks.length)
    {
      marks(i) match
      {
        case l: Left => count += 1
        case r: Right => count -= 1
        case p: Pick => picks(p.i) = count
      }

    }

    picks.foreach(c => print(c + " "))
    println
  }

}
