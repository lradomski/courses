def countChange(money: Int, coins: List[Int]): Int =
{
  var indent = 0;
  def count(money: Int, coins: List[Int]): Int =
  {
    def countCore(money: Int, coins: List[Int]): Int =
    {
      if (coins.isEmpty) 0
      else if (money > coins.head) count(money-coins.head, coins)+count(money,coins.tail)
      else if (money == coins.head) 1+count(money,coins.tail)
      else count(money, coins.tail)
    }

    indent = indent + 1;
    val r = countCore(money, coins)
    indent = indent - 1;
    r
  }

  if (coins.isEmpty) 0
  else if (money==0) 0
  else count(money, coins)
}

countChange(4,List(2,1))== 3
countChange(300,List(5,10,20,50,100,200,500)) == 1022
countChange(301,List(5,10,20,50,100,200,500)) == 0
countChange(300,List(500,5,50,100,20,200,10)) == 1022