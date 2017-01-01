import java.util.Scanner

import scala.annotation.tailrec

object Majority
{
  def majority(a: Array[Int]): Boolean =
  {
    case class Winner(index: Int, count: Int)
    {
      override def toString: String = "[v="+value+" (" + index + "): +" + count + "]"
      def exists = -1 != index

      def addCount(delta: Int): Winner = Winner(index, count + delta)

      def value = a(index)
    }

    object NoWinner extends Winner(-1, -1)
    {
      override def toString: String = "[]"

    }




    def count(key: Int, start: Int, end: Int): Int =
    {
      var count = 0
      for (i <- start to end) if (key == a(i)) count += 1
      count
    }

    def core(start: Int, end: Int): Winner =
    {
      if (start == end) Winner(start, 1)
      else
      {
        val mid = start + (end - start) / 2
        val size = (end+1 - start)
        val majority = size / 2 + 1

        var winner = core(start, mid)
        winner =
          if (winner.exists)
          {
            val otherCount = count(winner.value, mid + 1, end)
            if (winner.count + otherCount >= majority) winner.addCount(otherCount)
            else NoWinner
          }
          else NoWinner

        if (winner.exists) winner
        else
        {
          winner = core(mid + 1, end)
          if (winner.exists)
          {
            val otherCount = count(winner.value, start, mid)
            if (winner.count + otherCount >= majority) winner.addCount(otherCount)
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
