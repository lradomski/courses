import java.util.Scanner

object FiboPartSum
{
  def main(args: Array[String]): Unit =
  {
    val s = new Scanner(System.in)
    val m = s.nextLong
    val n = s.nextLong

    genSumDigitTable
    val out = partSumLastDigit(m, n)
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

  var fibos : Array[Int] = new Array[Int](0)
  var last : Int = 0

  def ensureArray(n : Int) =
  {
    if (n >= fibos.length)
    {
      fibos = new Array[Int](n+1 + fibos.length/2)
      fibos(0) = 0; fibos(1) = 1; fibos(2) = 1
      last = 2
    }

  }

  var findingPeriod = true
  var periodFound = false
  var period : Int = -1

  def fiboHugeMod(n: Long, mod: Int) : Int =
  {
    require(n > 0, "n > 0")
    require(n <= 10e18, "n <= 10e18")
    require(mod > 1, "mod > 1")
    require(mod <= 10e5, "10e5")

    ensureArray(100*1000)

    var i : Int = 2

    def fibo(n : Int) : Int =
    {
      ensureArray(n)

      if (n > last) {
        for (i <- last + 1 to n) fibos(i) = (fibos(i - 1) + fibos(i - 2)) % mod
        last = n
      }

      fibos(n)
    }

    def isPeriodStart(i : Int) : Boolean = (0 == fibo(i) && 1 == fibo(i+1))
    def periodEnd(i : Int, period : Int) : Boolean = i == 3*period
    def periodRepeats(i : Int, period : Int) : Boolean = fibo(i-period) == fibo(i)

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

  val max = 60
  val ss = new Array[Int](max+1)

  // from FiboSum
  def genSumDigitTable: Unit = {
    val f = new Array[Int](3)

    var s: Int = 0

    def idx(i : Long) : Int = (i % f.length).toInt


    var i : Long = 0
    f(0) = 0; ss(0) = 0
    f(1) = 1; ss(1) = 1; s = 1
    for (i <- 2 to max)
    {
      f( idx(i) ) = ( f( idx(i - 1) ) + f( idx(i - 2)) ) % 10


      s = (s + f(idx(i)))%10

      ss(i) = s
    }
  }

  def sumLastDigit(n: Long): Int = ss( (n % max).toInt )

  def partSumLastDigit(m: Long, n: Long): Int = {
    require(0 <= m, "0 <= m failed: " + m)
    require(m <= n, "m <= n failed: " + m + ", " + n)
    require(n <= 10e18, "n <= 10e18 failed: " + n)


    val f = new Array[Int](3)

    val max = 60
    var s: Int = 0
    val pss = new Array[Int](max+1)

    def idx(i : Long) : Int = (i % f.length).toInt


    var i : Long = 0
    f(0) = fiboHugeMod(m, 10); pss(0) = f(0)
    f(1) = fiboHugeMod(m+1L, 10); pss(1) = (pss(0) + f(1)) % 10

    s = pss(1)
    for (i <- 2 to max)
    {
      f( idx(i) ) = ( f( idx(i - 1) ) + f( idx(i - 2)) ) % 10


      s = (s + f(idx(i)))%10

      pss(i) = s
    }

    pss( ((n-m) % max).toInt )
  }
}
