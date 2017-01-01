import java.util.Scanner

import scala.annotation.tailrec

object Majority
{
  def majority(a: Array[Int]): Boolean =
  {
    case class Winner(index: Int, count: Int)
    {
      def exists = -1 != index

      def addCount(delta: Int): Winner = Winner(index, count + delta)

      def value = a(index)
    }

    object NoWinner extends Winner(-1, -1)




    def count(key: Int, start: Int, end: Int): Int =
    {
      var count = 0
      for (i <- start to end - 1) if (key == a(i)) count += a(i)
      count
    }

    def core(start: Int, end: Int): Winner =
    {
      if (start == end) Winner(start, 1)
      else
      {
        val mid = start + (end - start) / 2
        val majority = (end - start) / 2 + 1

        var leftWinner = core(start, mid)
        leftWinner =
          if (leftWinner.exists)
          {
            val rightCount = count(a(leftWinner.index), mid + 1, end)
            if (leftWinner.count + rightCount >= majority) leftWinner.addCount(rightCount)
            else NoWinner
          }

          else NoWinner

        if (leftWinner.exists) leftWinner
        else
        {
          val rightWinner = core(mid + 1, end)
          if (rightWinner.exists)
          {
            val leftCount = count(a(rightWinner.index), start, mid)
            if (rightWinner.count + leftCount >= majority) rightWinner.addCount(leftCount)
            else NoWinner
          }
          else NoWinner
        }
      }
    }

    core(0, a.length-1).exists
  }

  def main(args: Array[String]): Unit =
  {
    val s = new Scanner(System.in)

    val n = s.nextInt
    val a = new Array[Int](n)
    for (i <- 0 to n - 1) a(i) = s.nextInt

    println(if (majority(a)) 1 else 0)
  }

}
