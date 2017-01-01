import org.scalatest.FunSuite
import QuickSort3Way.qsort3way
import Utils._

class QuickSort3WayTest
extends FunSuite
{
  test("simple")
  {
    val a = List(5,4,3,2,1).toArray
    verify(qsort3way(a))

    verify(qsort3way(List(1).toArray))
    verify(qsort3way(List(2,1).toArray))
    verify(qsort3way(List(3,2,1).toArray))

    verify(qsort3way(List(1,2).toArray))
    verify(qsort3way(List(1,2,3).toArray))
    verify(qsort3way(List(1,2,3,4,5).toArray))
  }

  test("simple/2")
  {
    verify(qsort3way(List(60,118,118,166,60,209,289,118,209,747,821,289).toArray))
  }

  test("randomized")
  {
    val r = scala.util.Random
    for (c <- 1 to 1000)
      {
        val a = new Array[Int](r.nextInt(100))
        for (i <- 0 to a.length-1) a(i) = r.nextInt(1000)

        qsort3way(a)

        verify(a)
      }
  }

}
