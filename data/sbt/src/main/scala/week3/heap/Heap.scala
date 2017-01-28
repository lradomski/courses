package week3

import scala.annotation.tailrec

class Heap[T: Manifest](a: Array[T])(cond: (T,T) => Boolean)
{
  private var size = 0

  def this(n: Int)(cond: (T,T) => Boolean)
  {
    this(new Array[T](n))(cond)
  }

//  def newArray[T: Manifest](elem:T):Array[T] = {
//    new Array[T](1)
//  }

  def parent(i: Int) = (i - 1) / 2
  def left(i: Int) = 2 * i + 1
  def right(i: Int) = 2 * i + 2

  def top =
  {
    assert(!isEmpty, "top: empty")
    a(0)
  }

  def isEmpty = size == 0

  def takeTop =
  {
    val t = top
    if (size > 1)
    {
      a(0) = a(size-1)
      size -= 1
      siftDown(0)
    }
    else size -= 1

    t
  }

  def insert(v: T) =
  {
    assert(size < a.length, "insert: full")
    size += 1
    a(size-1) = v
    siftUp(size-1)
  }

  def length = size

  def toArray =
  {
    val out = new Array[T](size)
    while(!isEmpty) out(size-1) = takeTop
    out
  }

  private def isValid(i: Int) = 0 <= i && i < size

  private def verify(i: Int): Unit =
  {
    assert(isValid(i), "invalid index: " + i)
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
    verify(i)

    if (i == 0 || cond(a(parent(i)), a(i))) i
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

    if (isValid(ir) && !cond(a(i), a(ir)))
    {
      swap(i, ir)
      siftDown(ir)
    }
    else out
  }


}

object Heap
{
  implicit val cond = (l: Int, r: Int) => l > r

  def sorted[T: Manifest](a: Array[T])(cond: (T,T) => Boolean): Array[T] =
  {
    val h = a.foldLeft(new Heap[T](a.length)(cond))((h, v) =>
    {
      h.insert(v); h
    })

    h.toArray
  }

  def sort[T: Manifest](a: Array[T])(cond: (T,T) => Boolean): Array[T] =
  {
    val h = new Heap[T](a)(cond)
    h.size = a.length

    for (i <- a.length / 2 - 1 to 0 by -1)
    {
      h.siftDown(i)
    }


    while (!h.isEmpty) a(h.size-1) = h.takeTop

//    for (i <- 1 to a.length-1)
//    {
//      h.swap(0, h.size-1)
//      h.size -= 1
//      h.siftDown(0)
//    }

    a
  }
}

