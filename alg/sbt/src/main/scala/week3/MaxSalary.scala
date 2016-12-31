import java.util.Scanner

object MaxSalary
{
  def makeNumber(l: Int, r: Int): Int =
  {
    var tens = 10
    while (r / tens > 0) tens *= 10
    l * tens + r
  }

  def lessThan(l: Int, r: Int): Boolean =
  {
    assert(l <= 1e3.toInt)
    assert(r <= 1e3.toInt)

    makeNumber(l,r) < makeNumber(r, l)
  }

  def descending(l: Int, r: Int) = !lessThan(l,r)

  def main(args: Array[String]): Unit =
  {

    val s = new Scanner(System.in)

    val n = s.nextInt

    val nums = new Array[Int](n)
    for(i <- 0 to n-1) nums(i) = s.nextInt
    val sorted = nums.sortWith(descending)
    sorted.foreach(num => print(num))
    println
  }

}
