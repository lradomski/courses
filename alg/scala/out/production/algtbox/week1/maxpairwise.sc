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

getMaxPairwiseProduct(Array(10000, 900000))

scala.io.StdIn.readInt()
10

scala.io.StdIn.readInt() == 10
10
10
10




