import java.text.DecimalFormat
import java.util.Scanner

object DotProduct
{

  def main(args: Array[String]) =
  {
    val formatter = new DecimalFormat("#.####")
    val s = new Scanner(System.in);

    def readInts(n : Int) : Array[Int] =
    {
      val a = new Array[Int](n)
      for (i <- 0 to n-1) a(i) = s.nextInt()
      a
    }

    def readLongs(n : Int) : Array[Long] =
    {
      val a = new Array[Long](n)
      for (i <- 0 to n-1) a(i) = s.nextLong()
      a
    }

    def dotProduct(pageClicks: Array[Long], adProfits: Array[Long]): Long =
    {
      val clicksSorted = pageClicks.sortWith((l, r) => l > r)
      val profitsSorted = adProfits.sortWith((l, r) => l > r)

      var product: Long = 0
      for (i <- 0 to clicksSorted.length - 1)
      {
        product += clicksSorted(i) * profitsSorted(i)
      }

      product
    }


    val n = s.nextInt()
    val pageClicks = readLongs(n)
    val adProfits = readLongs(n)


    val a = dotProduct(pageClicks, adProfits)
    println(a)
  }


}
