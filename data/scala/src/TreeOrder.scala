import java.io.File
import java.util.Scanner

/**
  * Created by luke on 12/27/16.
  */
object TreeOrder
{



  def main(args: Array[String]) =
  {
    case class Vertex(key: Long, left: Int, right: Int)

    val s: Scanner = if (args.isEmpty) new Scanner(System.in) else new Scanner(new File(args(0)))

    val n = s.nextInt()

    def readVertices: Array[Vertex] =
    {
      val v = new Array[Vertex](n)
      for(i <- 0 to n-1) v(i) = Vertex(s.nextLong, s.nextInt, s.nextInt)
      v
    }

    val v = readVertices
  }
}
