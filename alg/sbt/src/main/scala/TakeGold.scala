import java.util.Scanner

object TakeGold
{
  def main(args: Array[String]) =
  {
    val s = new Scanner(System.in)

    val n = s.nextInt()
    val capacity = s.nextInt()

    val bars = for (i <- 1 to n) yield s.nextInt

  }
}
