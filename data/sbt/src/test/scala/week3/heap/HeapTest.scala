package week3.heap

import org.scalatest.FunSuite
import org.scalatest.prop.Checkers
import week3._


class HeapTest
extends FunSuite with Checkers
{
  //val cond: (l: Int, r: Int) => l>r
  test("heap sort - new array")
  {
    check((a: Array[Int]) => (Heap.sorted(a)(_>_) zip a.sorted).forall(lr => lr._1 == lr._2))
  }

  test("heap sort - in place")
  {
    check((a: Array[Int]) => (Heap.sort(a)(_>_) zip a.sorted).forall(lr => lr._1 == lr._2))
  }
}
