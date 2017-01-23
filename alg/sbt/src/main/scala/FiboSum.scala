import java.util.Scanner

import scala.annotation.tailrec

object FiboSum
{
  def sumLastDigit(n: Long): Int = {
    require(n >= 0, "n >= 0")
    require(n <= 1e14, "n <= 1e14")


    val f = new Array[Int](3)

    val max = 60
    var s: Int = 0
    val ss = new Array[Int](max+1)

    def idx(i : Long) : Int = (i % f.length).toInt


    var i : Long = 0
    f(0) = 0; ss(0) = 0
    f(1) = 1; ss(1) = 1; s = 1
    for (i <- 2 to max)
    {
      f( idx(i) ) = ( f( idx(i - 1) ) + f( idx(i - 2)) ) % 10


      s = (s + f(idx(i)))%10

      ss(i) = s

      //print(s + " ")
    }
    //println

//    for (i <- 0 to 60)
//      {
//        assert(ss(i) == ss(60+i))
//        assert(ss(60+i) == ss(120+i))
//      }

    ss( (n % max).toInt )
    //ss(n.toInt)
  }



  def main(args: Array[String]): Unit =
  {
    val s = new Scanner(System.in)
    val n = s.nextLong

    val out = sumLastDigit(n)
    println(out)
  }

}

/*
0 1 3 6 1 9 2 3 7 2 1 5 8 5 5 2 9 3 4 9 5 6 3 1 6 9 7 8 7 7 6 5 3 0 5 7 4 3 9 4 5 1 8 1 1 4 7 3 2 7 1 0 3 5 0 7 9 8 9 9
0 1 3 6 1 9 2 3 7 2 1 5 8 5 5 2 9 3 4 9 5 6 3 1 6 9 7 8 7 7 6 5 3 0 5 7 4 3 9 4 9

 */