import java.util.Scanner

import scala.annotation.tailrec

object TakeGold
{

  @tailrec
  def makeValues(max: Int, bars: List[Int], values: Set[Int]): Set[Int] = bars match
  {
    case Nil => values
    case x :: xs => makeValues(max, xs, if (x<=max) values ++ values.map(_+x).filter(_<=max) else values)
  }


  def main(args: Array[String]): Unit =
  {
    val s = new Scanner(System.in)

    val capacity = s.nextInt
    val bars = for (i <- 1 to s.nextInt) yield s.nextInt

    val taken = makeValues(capacity, bars.toList, Set(0)).max
    println(taken)

  }
}
