import java.util.Scanner

object FiboLastDigit {
  def main(args: Array[String]) = {
    val s = new Scanner(System.in);

    val n = s.nextInt();
    println( fibonacci(n) );
  }

  def fibonacci(n: Long): Int = {
    require(n >= 0, "n >= 0")
    require(n <= 10*1000*1000, "n <= 10*1000*1000")

    val f = new Array[Int](3)

    def idx(i : Long) : Int = (i % f.length).toInt


    var i : Long = 0
    while  (i <= n)
    {
      if (i <= 1) f( idx(i) ) = i.toInt
      else {
        f( idx(i) ) = ( f( idx(i - 1) ) + f( idx(i - 2)) ) % 10
      }
      i = i + 1;
    }

    f(idx(n))
  }

}
