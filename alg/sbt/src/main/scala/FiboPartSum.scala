import java.util.Scanner

object FiboPartSum
{
  def main(args: Array[String]): Unit =
  {
    val s = new Scanner(System.in)
    val n = s.nextLong

    val out = sumLastDigit(n)
    println(out)
  }

  def fiboMod(n: Long, mod : Int): Int = {
    require(n >= 0, "n >= 0")
    require(n <= 10*1000*1000, "n <= 10*1000*1000")

    val f = new Array[Int](3)

    def idx(i : Long) : Int = (i % f.length).toInt


    var i : Long = 0
    while  (i <= n)
    {
      if (i <= 1) f( idx(i) ) = i.toInt
      else {
        f( idx(i) ) = ( f( idx(i - 1) ) + f( idx(i - 2)) ) % mod
      }
      i = i + 1;
    }

    f(idx(n))
  }

  def fiboHugeMod(n: Long, mod: Int) : Int =
  {
    require(n > 0, "n > 0")
    require(n <= 10e18, "n <= 10e18")
    require(mod > 1, "mod > 1")
    require(mod <= 10e5, "10e5")

    var f : Array[Int] = new Array[Int](0)
    var last : Int = 0

    def ensureArray(n : Int) =
    {
      if (n >= f.length)
      {
        f = new Array[Int](n+1 + f.length/2)
        f(0) = 0; f(1) = 1; f(2) = 1
        last = 2
      }

    }

    ensureArray(100*1000)

    def fibo(n : Int) : Int =
    {
      ensureArray(n)

      if (n > last) {
        for (i <- last + 1 to n) f(i) = (f(i - 1) + f(i - 2)) % mod
        last = n
      }

      f(n)
    }

    var i : Int = 2
    var period : Int = -1

    def isPeriodStart(i : Int) : Boolean = (0 == fibo(i) && 1 == fibo(i+1))
    def periodEnd(i : Int, period : Int) : Boolean = i == 3*period
    def periodRepeats(i : Int, period : Int) : Boolean = fibo(i-period) == fibo(i)

    var findingPeriod = true
    var periodFound = false

    while (i < n  && !periodFound)
    {
      if (findingPeriod)
      {
        if (isPeriodStart(i))
        {
          period = i
          ensureArray(i)
          findingPeriod = false
        }
      }
      else
      {
        if (periodRepeats(i, period)) periodFound = periodEnd(i, period) else findingPeriod = true
      }

      i+=1;
    }

    if (periodFound) fibo( (n % period).toInt ) else fiboMod(n, mod)
  }

  // from FiboSum
  def sumLastDigit(n: Long): Int = {
    require(n >= 0, "n >= 0")
    //require(n <= 1e14, "n <= 1e14")


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

}
