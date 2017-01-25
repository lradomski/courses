import java.io.File
import java.util.Scanner

/**
  * Created by luke on 12/27/16.
  */
object Rope
{
  def main(args: Array[String]) =
  {
    case class Op(left: Int, right: Int, insert: Int)

    val s: Scanner = if (args.isEmpty) new Scanner(System.in) else new Scanner(new File(args(0)))

    val text = s.next()
    val n = s.nextInt()

    for(i <- 0 to n-1)
    {
      val op = Op(s.nextInt, s.nextInt, s.nextInt)
    }
  }
}
