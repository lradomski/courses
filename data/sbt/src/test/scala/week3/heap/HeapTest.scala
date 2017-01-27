package week3.heap

import org.scalatest.FunSuite
import org.scalatest.prop.Checkers
import week3.Heap


class HeapTest
extends FunSuite with Checkers
{
  test("heap sort - new array")
  {
    //check((a: List[Int], b: List[Int]) => a.size < (a ::: b).size)
    check((a: Array[Int]) => (Heap.sort(a) zip a.sorted).forall(lr => lr._1 == lr._2))
  }
}
