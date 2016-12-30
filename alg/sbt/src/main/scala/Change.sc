change(94)
change(194)


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