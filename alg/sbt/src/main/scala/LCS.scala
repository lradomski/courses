import java.util.Scanner

object LCS
{
  def main(args: Array[String]) =
  {
    val s = new Scanner(System.in)

    val a = (for (i <- 1 to s.nextInt) yield s.nextInt).toArray
    val b = (for (i <- 1 to s.nextInt) yield s.nextInt).toArray
    val c = (for (i <- 1 to s.nextInt) yield s.nextInt).toArray
  }
}
