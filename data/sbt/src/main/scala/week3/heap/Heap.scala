package week3

import scala.annotation.tailrec

class Heap(n: Int)
{

  private var size = 0
  private val a = new Array[Int](n + 1) // numbering 1..n (leave 0)


  private def parent(i: Int) = i / 2

  private def left(i: Int) = 2 * i

  private def right(i: Int) = 2 * i + 1

  def top =
  {
    assert(size > 0, "top: empty")
    a(1)
  }

  def takeTop =
  {
    val t = top
    if (size > 1)
    {
      a(1) = a(size)
      size -= 1
      siftDown(1)
    }
    else size -= 1

    t
  }

  def insert(v: Int) =
  {
    assert(size < n, "insert: full")
    size += 1
    a(size) = v
    siftUp(size)
  }

  def length = size
  //def toArray = a.clone

  private def isValid(i: Int) = 1 <= i && i <= size

  private def verify(i: Int): Unit =
  {
    assert(isValid(i), "invalid index")
  }

  private def cond(vParent: Int, vChild: Int) = vParent < vChild

  private def isHeapUp(i: Int): Boolean =
  {
    verify(i)
    if (i == 1) true else cond(a(parent(i)), a(i))
  }

  private def swap(i: Int, j: Int): Unit =
  {
    verify(i)
    verify(j)
    val v = a(i)
    a(i) = a(j)
    a(j) = v
  }

  @tailrec
  private def siftUp(i: Int): Int =
  {
    if (isHeapUp(i)) i
    else
    {
      val ip = parent(i)
      swap(i, parent(i))
      siftUp(ip)
    }
  }


  private def siftDown(i: Int): Int =
  {
    verify(i)

    val il = left(i)
    val ir = right(i)
    val out = if (isValid(il) && !cond(a(i), a(il)))
    {
      swap(i, il)
      siftDown(il)
    }
    else i

    if (isValid(ir)  && !cond(a(i), a(ir)))
    {
      swap(i, ir)
      siftDown(ir)
    }
    else out
  }


}

object Heap
{
  def sort(a: Array[Int]): Array[Int] =
  {
    val h = a.foldLeft(new Heap(a.length))( (h,v) => { h.insert(v); h })
    val hl = h.length
    val out = for (i <- 1 to hl) yield h.takeTop
    out.toArray
  }

  def sortInPlace(a: Array[Int], cond: (Int,Int) => Boolean): Unit =
  {
    def parent(i: Int) = i / 2 - 1
    def left(i: Int) = 2 * i - 1
    def right(i: Int) = 2 * i + 1 - 1

    def isValid(i: Int) = 0 <= i && i < a.length

    def verify(i: Int): Unit =
    {
      assert(isValid(i), "0 <= i  &&  i <= size")
    }

    def swap(i: Int, j: Int): Unit =
    {
      verify(i)
      verify(j)
      val v = a(i)
      a(i) = a(j)
      a(j) = v
    }


    def siftDown(i: Int): Int =
    {
      verify(i)

      val il = left(i)
      val ir = right(i)

      val out = if (isValid(il) && !cond(a(i), a(il)))
      {
        swap(i, il)
        siftDown(il)
      }
      else i

      if (isValid(ir)  && !cond(a(i), a(ir)))
      {
        swap(i, ir)
        siftDown(ir)
      }
      else out
    }

    for (i <- a.length/2-1 to 0 by -1) siftDown(i)
  }


}
