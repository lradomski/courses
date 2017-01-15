import java.util.Scanner

import scala.annotation.tailrec

object SimpleCalc
{
  case class OpsNums(ops: Int, nums: List[Int])

  def calc(n: Int): (Int, List[Int]) =
  {
    def minus1(i: Int) = Some(i-1)
    def div2(i: Int) = if (i % 2 == 0) Some(i / 2) else None
    def div3(i: Int) = if (i % 3 == 0) Some(i / 3) else None

    def ops(i: Int) = List(minus1(i), div2(i), div3(i))

    val a = new Array[Int](n + 1)

    a(0) = 0
    a(1) = 0 // start


    for (i <- 2 to n)
    {
      a(i) = ops(i).withFilter(_ != None).map(r => a(r.get)).min + 1
    }

    @tailrec
    def seq(i: Int, acc: List[Int]): List[Int] =
    {
      if (i > 1)
      {
        val prev = ops(i).withFilter(_ != None).map(r => (a(r.get), r.get)).minBy(_._1)._2
        seq(prev, i :: acc)
      }
      else 1 :: acc
    }

    (a(n), seq(n, List()))
  }

  def main(args: Array[String]): Unit =
  {
    val s = new Scanner(System.in)

    val n = s.nextInt
    val r = calc(n)
    println(r._1)
    r._2.foreach(i => print(i + " ")); println
  }


}
