import java.io.File
import java.util.Scanner

import scala.annotation.tailrec
import scala.collection.mutable

// copied from week3/Heap.scala:
class Heap_MH[T: Manifest](a: Array[T])(cond: (T, T) => Boolean)
{
  var size = 0

  def this(n: Int)(cond: (T, T) => Boolean)
  {
    this(new Array[T](n))(cond)
  }

  var q : mutable.Queue[(Int,Int)] = null

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
      a(0) = a(size - 1)
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
    a(size - 1) = v
    siftUp(size - 1)
  }

  def length = size

  def toArray =
  {
    val out = new Array[T](size)
    while (!isEmpty) out(size - 1) = takeTop
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
    if (q != null) q.enqueue((i,j))
  }

  @tailrec
  private def siftUp(i: Int): Int =
  {
    verify(i)

    if (i == 0 || cond(a(parent(i)), a(i))) i
    else
    {
      val ip = parent(i)
      swap(i, ip)
      siftUp(ip)
    }
  }


  @tailrec
  final def siftDown(i: Int): Int =
  {
    /*
      maxIndex ← i

      ℓ ← LeftChild(i)
      if ℓ ≤ size and H[ℓ] > H[maxIndex]:
        maxIndex ← ℓ

      r ← RightChild(i)
      if r ≤ size and H[r] > H[maxIndex]:
        maxIndex ← r

      if i ̸= maxIndex:
        swap H[i] and H[maxIndex]
        SiftDown(maxIndex)
     */

    verify(i)

    val il = left(i)
    val ir = right(i)

    var imax = i

    if (isValid(il) && !cond(a(i), a(il)))
    {
      imax = il
    }

    if (isValid(ir) && !cond(a(imax), a(ir)))
    {
      imax = ir
    }

    if (imax != i)
    {
      swap(i, imax)
      siftDown(imax)
    }
    else i
  }


}

object MakeHeap {

  def main(args: Array[String]) =
  {
    //      for (i <- 1 to 24)
    //        {
    //          val name = "/Users/luke/git/courses/data/download/Starters PA1/tree_height/tests/%02d".format(i)
    //          val s : Scanner = new Scanner( new File(name) )
    //          val n = s.nextInt();
    //          val vertices = readVertices(s, n)
    //          val out = treeHeight(vertices)
    //
    //          val a = new Scanner( new File(name+".a")).nextInt();
    //          val res = (a==out);
    //          val line = "%02d".format(i)
    //          println(s"${line}: ${res} (${out},${a})");
    //        }

    val s : Scanner = if (args.isEmpty) new Scanner(System.in) else new Scanner( new File(args(0)) )
    val n = s.nextInt();
    val vertices = (for (i <- 1 to n) yield s.nextInt).toArray

    //val vertices : Array[Int] = Array(5, 4, 3, 2, 1)

    val swaps = new scala.collection.mutable.Queue[(Int, Int)]()
    makeHeap(vertices, swaps)

    println(swaps.length)
    swaps.foreach(ij => println(ij._1 + " " + ij._2))

    // /Users/luke/git/courses/data/download/Programming-Assignment-2/make_heap/tests/04
  }


  def makeHeap(a : Array[Int], swaps: scala.collection.mutable.Queue[(Int,Int)]) =
  {
    val h = new Heap_MH[Int](a)(_<_) // min-heap
    h.q = swaps
    h.size = a.length
    for (i <- a.length / 2 - 1 to 0 by -1) h.siftDown(i)

  }

}
