import java.util.Scanner

import scala.annotation.tailrec

object Prizes
{
  @tailrec
  def prizes(n: Int, last: Int, acc: List[Int]): List[Int] =
  {
    if (n == 0) acc
    else
    {
      val next = last+1
      val choice = if (n-next >= next+1)  next else n

      prizes(n-choice, choice, acc ::: List(choice))
    }
  }

  def main(args: Array[String]): Unit =
  {
    val s = new Scanner(System.in)

    val total = s.nextInt

    val ps = prizes(total, 0, List[Int]())

    println(ps.length)
    ps.foreach(p => print(p + " "))
    println
  }

}
