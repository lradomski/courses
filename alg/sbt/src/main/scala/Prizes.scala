import java.util.Scanner

import scala.annotation.tailrec

object Prizes
{
  def prizes(n: Int): List[Int] = prizesIter(n) //prizesRec(n)

  def prizesFast(total: Int): Unit =
  {
    def arithSum(first: Int, last: Int, count: Int): Long = (((first.toLong + last.toLong) * count.toLong) / 2)
    def step(s: Int) = if (s == 0) 1 else s

    @tailrec
    def core(start: Int, end: Int): Int =
    {
      if (start == end) start
      else if (start + 1 == end)
      {
        val sum = arithSum(1, start, start)
        val diff = total - sum

        if (diff == end) end
        else if (diff > end) end
        else start - 1
      }
      else
      {
        val next = step((end - start) / 2)
        val count = start + next
        val sum = arithSum(1, count, count)
        //println("// " + count + " [" + start + ", " + end + "] =" + sum)

        if (sum < total)
        {
          val diff = total - sum

          if (diff == count + 1) count + 1
          else if (diff > count + 1)
          {
            core(count, end)
          }
          else count - 1
        }
        else if (sum > total)
        {
          core(start, count)
        }
        else count
      }
    }


    val count = core(1, total)
    val last = total - arithSum(1, count, count)
    assert(last == 0 || last > count + 1)
    println(count + (if (last > 0) 1 else 0))
    var sum = 0
    for (i <- 1 to count)
    {
      print(i + " ")
      //sum += i
    }
    if (last > 0)
    {
      println(last)
      //assert(sum + last == total)
    }
    else
    {
      //assert(sum == total)
      println
    }
  }

  def prizesRec(total: Int): List[Int] =
  {
    @tailrec
    def core(n: Int, last: Int, acc: List[Int]): List[Int] =
    {
      if (n == 0) acc
      else
      {
        val next = last + 1
        val choice = if (n - next >= next + 1) next else n

        core(n - choice, choice, acc ::: List(choice))
      }
    }

    core(total, 0, List[Int]())
  }


  def prizesIter(total: Int): List[Int] =
  {
    var n = total
    var prizes = List[Int]()
    var last = 0

    while (n > 0)
    {
      val next = last + 1
      val choice = if (n - next >= next + 1) next else n
      n -= choice
      last = choice
      prizes = prizes ::: List(choice)
    }

    prizes
  }

  def main(args: Array[String]): Unit =
  {
    val s = new Scanner(System.in)

    //    for(i <- 1 to 44719) print(i + " ")
    //    println(83160)

    val total = s.nextInt

    //    val ps = prizes(total)
    //
    //    println(ps.length)
    //    ps.foreach(p => print(p + " "))
    //    println

    prizesFast(total)
  }

}
