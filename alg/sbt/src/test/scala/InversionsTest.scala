import org.scalatest.FunSuite
import Utils._
import Inversions._

class InversionsTest
extends FunSuite
{
  def sort(a: Array[Int]): Array[Int] = inversions(a).items.toArray

  test("merge")
  {
    val r1 = merge(Result(List(1),0), Result(List(2),0), EmptyResult)
    assert(r1.invs == 0)
    verify(r1.items, List(1,2))

    val r2 = merge(Result(List(2),0), Result(List(1),0), EmptyResult)
    assert(r2.invs == 1)
    verify(r2.items, List(1,2))

    val r3 = merge(Result(List(2,3,4),0), Result(List(1),0), EmptyResult)
    assert(r3.invs == 3)
    verify(r3.items, List(1,2,3,4))

    val r4 = merge(Result(List(2,3,4),0), Result(List(1,2),0), EmptyResult)
    assert(r4.invs == 5)
    verify(r4.items, List(1,2,2,3,4))

    val r5 = merge(Result(List(2,3,9),0), Result(List(2,9),0), EmptyResult)
    assert(r5.invs == 2)
    verify(r5.items, List(2,2,3,9,9))


  }

  test("simple")
  {
    val a = List(5,4,3,2,1).toArray
    verify(sort(a))

    verify(sort(List(1).toArray))
    verify(sort(List(2,1).toArray))
    verify(sort(List(3,2,1).toArray))

    verify(sort(List(1,2).toArray))
    verify(sort(List(1,2,3).toArray))
    verify(sort(List(1,2,3,4,5).toArray))
  }

  test("simple/2")
  {
    //verify(sort(List(60,118,118,166,60,209,289,118,209,747,821,289).toArray))
    val l = inversions(List(284,534,950,312).toArray)
    verify(l.items.toArray)
  }

  test("randomized")
  {
    val r = scala.util.Random
    for (c <- 1 to 1000)
    {
      val a = new Array[Int](r.nextInt(100))
      for (i <- 0 to a.length-1) a(i) = r.nextInt(1000)



      verify(sort(a))
    }
  }

}
