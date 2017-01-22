import java.util.Scanner

import scala.annotation.tailrec

object FiboSum
{
  def sumLastDigit(n: Long): Int = {
    require(n >= 0, "n >= 0")
    require(n <= 10*1000*1000, "n <= 10*1000*1000")

    val f = new Array[Int](3)
    var s: Int = 0

    def idx(i : Long) : Int = (i % f.length).toInt


    var i : Long = 0
    while  (i <= n)
    {
      if (i <= 1) f( idx(i) ) = i.toInt
      else {
        f( idx(i) ) = ( f( idx(i - 1) ) + f( idx(i - 2)) ) % 10
      }

      s = (s + f(idx(i)))%10
      i = i + 1;
    }

    s
  }

//  @tailrec
//  def sumLastDigit(n: Long): Int =
//  {
//    //(sum(n-1) + n%10)%10
//    if (n == 0) 0
//    else if (n == 1) 1
//    else
//    {
//      val d = (n % 10).toInt
//      (d + sumLastDigit(n-1))%10
//    }
//
//  }

  def main(args: Array[String]): Unit =
  {
    val s = new Scanner(System.in)
    val n = s.nextLong

    val out = sumLastDigit(n)
    println(out)
  }

}
