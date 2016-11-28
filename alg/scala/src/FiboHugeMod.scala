import java.util.Scanner


object FiboHugeMod {
  def main(args: Array[String]) = {
    val s = new Scanner(System.in);

    val n = s.nextLong();
    val mod = s.nextInt();
    println( fiboHugeMod(n, mod) );

//    println( fiboHugeMod(239, 1000) )
//    println( fiboHugeMod(100, 100*1000) )
//    println( fiboHugeMod(10, 2) )
//    println( fiboHugeMod(99999999999999999L, 2) )
//    println( fiboHugeMod(99999999999999999L, 100*1000) )
  }

  def fiboHugeMod(n: Long, mod: Int) : Int =
  {
    require(n > 0, "n > 0")
    require(n <= 10e18, "n <= 10e18")
    require(mod > 1, "mod > 1")
    require(mod <= 10e5, "10e5")

    //val max : Int = Math.min(n, 4*100*1000L).toInt

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

    do
    {
      while(i < n && (0 != fibo(i) || 1 != fibo(i+1)))
      {
//          if (i % 500 == 0) println(i)
        i+=1
      }

      if (i < n)
      {
        period = i
        ensureArray(3*period)
        println("Period: " + period + ", " + fibo(i) + ", " + fibo(i+1))

        i+=2

        while (i < n && i < 3*period && fibo(i-period) == fibo(i))
        {
//          if (i % 500 == 0) println(i)
          i+=1

        }
      }
    } while (i != 3*period && i < n)

    if (i == 3*period) fibo( (n % period).toInt ) else fiboMod(n, mod)
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

}
