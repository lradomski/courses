import java.util.Scanner

object Change
{
  def change(m: Int): Int =
  {
    assert(1 <= m)
    assert(m <= 10e3)

    var money = m;
    var count: Int = 0

    val coins = Array(10, 5, 1)
    coins.foreach(coin =>
    {
      if (0 < money)
      {
        val coinCount = money / coin
        if (0 < coinCount)
        {
          count += coinCount
          money -= coinCount * coin
        }
      }
    })

    count
  }


  def main(args: Array[String]) =
  {
    val s = new Scanner(System.in);

    val m = s.nextInt();
    //val m = 100
    println(change(m));
  }


}
