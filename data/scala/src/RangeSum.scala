import java.io.File
import java.util.Scanner

/**
  * Created by luke on 12/27/16.
  */
object RangeSum
{

  def main(args: Array[String]) =
  {
    case class Op(op: String, left: Long, right: Long)

    val s: Scanner = if (args.isEmpty) new Scanner(System.in) else new Scanner(new File(args(0)))

    val n = s.nextInt()

    for(i <- 0 to n-1)
    {
      val op = Op(s.next(), s.nextLong, s.nextLong)
    }
  }
}
