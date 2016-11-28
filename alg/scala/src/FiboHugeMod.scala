import java.util.Scanner


object FiboHugeMod {
  def main(args: Array[String]) = {
    val s = new Scanner(System.in);

    val n = s.nextLong();
    val mod = s.nextInt();
    println( fiboHugeMod(n, mod) );
//    println( fiboHugeMod(239, 1000) )
  }

  def fiboHugeMod(n: Long, mod: Int) : Int =
  {
    require(n > 0, "n > 0")
    require(n <= 10e18, "n <= 10e18")
    require(mod > 1, "mod > 1")
    require(mod <= 10e5, "10e5")

    def fibo(n : Long) : Int = fiboMod(n, mod)

    var i : Long = 0
    var period : Long = 0

    do
    {
      while(0 != fibo(i) && 1 != fibo(i+1)) i+=1

      period = i
      i+=2

      while (i < 3*period && fibo(i-period) == fibo(i)) i+=1
    } while (! (i == 3*period))


    fibo(n % period)
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
