import org.scalatest.FunSuite
import Utils._
import Inversions._

class InversionsTest
extends FunSuite
{
  def sort(a: Array[Int]): Array[Int] = inversions(a).items

  def verify2(a: Array[Int]): Unit =
    {
      val orig = a.clone()
      val invs = invSimple(a)
      val r = inversions(a)
      assert(r.invs == invs, Utils.toStr(orig))
      verify(r.items)
    }

  def invSimple(a: Array[Int]): Int =
  {
    var c = 0
    for (i <- 0 to a.length-1)
      for (j <- i to a.length-1)
        if (a(i) > a(j)) c += 1

    c
  }

  test("merge-list")
  {
//    val r1 = merge(Result(List(1),0), Result(List(2),0), EmptyResult)
//    assert(r1.invs == 0)
//    verify(r1.items, List(1,2))
//
//    val r2 = merge(Result(List(2),0), Result(List(1),0), EmptyResult)
//    assert(r2.invs == 1)
//    verify(r2.items, List(1,2))
//
//    val r3 = merge(Result(List(2,3,4),0), Result(List(1),0), EmptyResult)
//    assert(r3.invs == 3)
//    verify(r3.items, List(1,2,3,4))
//
//    val r4 = merge(Result(List(2,3,4),0), Result(List(1,2),0), EmptyResult)
//    assert(r4.invs == 5)
//    verify(r4.items, List(1,2,2,3,4))
//
//    val r5 = merge(Result(List(2,3,9),0), Result(List(2,9),0), EmptyResult)
//    assert(r5.invs == 2)
//    verify(r5.items, List(2,2,3,9,9))

    {
      val r = mergeList(ResultList(List(7,8,9),3), ResultList(List(1,2,3),3), EmptyResultList)
        assert(r.invs == 15)
        verify(r.items, List(1,2,3,7,8,9))

    }


  }

  test("merge")
  {
        val r1 = merge(Result(Array(1),0), Result(Array(2),0))
        assert(r1.invs == 0)
        verify(r1.items, Array(1,2))

        val r2 = merge(Result(Array(2),0), Result(Array(1),0))
        assert(r2.invs == 1)
        verify(r2.items, Array(1,2))

        val r3 = merge(Result(Array(2,3,4),0), Result(Array(1),0))
        assert(r3.invs == 3)
        verify(r3.items, Array(1,2,3,4))

        val r4 = merge(Result(Array(2,3,4),0), Result(Array(1,2),0))
        assert(r4.invs == 5)
        verify(r4.items, Array(1,2,2,3,4))

        val r5 = merge(Result(Array(2,3,9),0), Result(Array(2,9),0))
        assert(r5.invs == 2)
        verify(r5.items, Array(2,2,3,9,9))

    {
      val r = merge(Result(Array(7,8,9),3), Result(Array(1,2,3),3))
      assert(r.invs == 15)
      verify(r.items, Array(1,2,3,7,8,9))

    }


  }

  test("simple")
  {
    val a = List(5,4,3,2,1).toArray
    verify(sort(a))

    verify2(Array(1))
    verify2(Array(2,1))
    verify2(Array(3,2,1))

    verify2(Array(1,2))
    verify2(Array(1,2,3))
    verify2(Array(1,2,3,4,5))
  }

  test("simple/2")
  {
    //verify(sort(List(60,118,118,166,60,209,289,118,209,747,821,289).toArray))
    val l = inversionsList(List(284,534,950,312).toArray)
    verify(l.items.toArray)
  }

  test("simple/3")
  {
//    {
//      val a = Array(1,2,1,2)
//      verify2(a)
//    }

    {
      val a = Array(158, 337, 663, 337, 358, 337, 568, 562)
      verify2(a)
    }
  }

  test("randomized")
  {
    val r = scala.util.Random
    for (c <- 1 to 100000)
    {
      val a = new Array[Int](r.nextInt(10))
      for (i <- 0 to a.length-1) a(i) = r.nextInt(1000)

      verify2(a)
    }
  }

}
