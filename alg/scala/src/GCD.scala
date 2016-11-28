import java.util.Scanner

object GCD {
  def main(args: Array[String]) = {
    val s = new Scanner(System.in);

    val a = s.nextLong();
    val b = s.nextLong();
    println( gcd(a,b) );
  }

  def gcd(a: Long, b:Long) : Long =
  {
    require(0 <= a, "0 <= a")
    require(0 <= b, "0 <= b")
    require(a <= 10e9, "a <= 10e9")
    require(b <= 10e9, "b <= 10e9")

    if (a == 1 || b == 1) 1
    else if (a == 0) b
    else if (b == 0) a
    else if (a > b) gcd(b, a % b)
    else gcd(a, b % a)
  }
}
