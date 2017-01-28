import java.io.File
import java.util.Scanner

import scala.annotation.tailrec

// copied from week3/Heap.scala:
class Heap_JQ[T: Manifest](a: Array[T])(cond: (T, T) => Boolean)
{
  private var size = 0

  def this(n: Int)(cond: (T, T) => Boolean)
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
  private def siftDown(i: Int): Int =
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

object JobQueue
{

  def main(args: Array[String]) =
  {

    def readInts(s: Scanner, n: Int): Array[Int] =
    {
      val tree = new Array[Int](n)
      for (i <- 0 to n - 1)
      {
        tree(i) = s.nextInt()
      }
      tree
    }

    def readLongs(s: Scanner, n: Int): Array[Long] =
    {
      val tree = new Array[Long](n)
      for (i <- 0 to n - 1)
      {
        tree(i) = s.nextLong()
      }
      tree
    }

    val s: Scanner = if (args.isEmpty) new Scanner(System.in) else new Scanner(new File(args(0)))
    val n = s.nextInt();
    val m = s.nextInt();

    val jobs = readLongs(s, m)

    //val jobs : Array[Long] = List(5, 4, 3, 2, 1).toArray

    val threads = jobQueue(n, jobs)

    assert(threads.length == jobs.length)

    if (args.isEmpty)
    {
      for (i <- 0 to jobs.length - 1)
      {
        println(threads(i) + " " + jobs(i))
      }
    }
    else
    {
      val s2 = new Scanner(new File(args(0) + ".a"))

      var i = 0
      while (i < jobs.length && s2.hasNext)
      {
        val t = s2.nextInt()
        val s = s2.nextLong()
        val ok = if (threads(i) == t && jobs(i) == s) "  " else "> "
        println(ok + threads(i) + " " + jobs(i) + " / " + t + " " + s)
        assert(threads(i) == t, "threads(i) == t")
        assert(jobs(i) == s, "jobs(i) == s")
        i += 1
      }
    }


  }

  def jobQueue(n: Int, jobs: Array[Long]): Array[Int] =
  {
    case class Pending(thread: Int, job: Int, start: Long) extends Ordered[Pending]
    {
      override def compare(that: Pending): Int =
      {
        if (this.end == that.end) -1 * this.thread.compare(that.thread)
        else -1 * this.end.compare(that.end)
      }

      def end = start + jobs(job)
    }


    val threads = new Array[Int](jobs.length) // record of which thread processed which job

    //    val jobsPending = new scala.collection.mutable.PriorityQueue[Pending]() //(Ordering.by(p => -1*(p.start+jobs(p.job))))
    val cond = (l: Pending, r: Pending) => l.compare(r) > 0
    val jobsPending = new Heap_JQ[Pending](jobs.length)(cond)

    var time: Long = 0

    var i = 0 // job idx

    while (i < n && i < jobs.length)
    {
      //      jobsPending += Pending(i, i, time)
      jobsPending.insert(Pending(i, i, time))
      i += 1
    }

    while (!jobsPending.isEmpty)
    {
      //      val done = jobsPending.dequeue()
      val done = jobsPending.takeTop

      time = done.start + jobs(done.job)
      threads(done.job) = done.thread
      jobs(done.job) = done.start

      if (i < jobs.length)
      {
        //        jobsPending += Pending(done.thread, i, time)
        jobsPending.insert(Pending(done.thread, i, time))
        i += 1
      }
    }


    threads
  }


}
