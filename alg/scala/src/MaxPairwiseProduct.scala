import java.util.Scanner


object MaxPairwiseProduct {
  def main(args: Array[String]) = {
    val s = new Scanner(System.in);

    val n = s.nextInt();
    val numbers : Array[Int] = new Array[Int](n);
    for (i <- 0 to n-1) numbers(i) = s.nextInt()
    println( getMaxPairwiseProduct(numbers) );
  }

  def getMaxPairwiseProduct (numbers: Array[Int]) : Long =
  {
    var top1 : Long = 0;
    var top2 : Long = 0;

    for (n <- numbers)
    {
      if (n > top1)
      {
        top2 = top1
        top1 = n
      }
      else if (n > top2)
      {
        top2 = n
      }
    }

    top1*top2
  }

}
