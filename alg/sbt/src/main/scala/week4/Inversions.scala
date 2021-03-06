import java.util.Scanner

import scala.annotation.tailrec

object Inversions
{

  case class Result(items: Array[Int], invs: Long)
  {
    override def toString: String = "[ (" + items.foldLeft("")((s, i) => s + i + ",") + "), " + invs + "]"
  }

  def merge(l: Result, r: Result): Result =
  {
    val out = new Array[Int](l.items.length + r.items.length)

    var i = 0

    var li = 0
    val lend = l.items.length

    var ri = 0
    val rend = r.items.length

    var invs: Long = 0L

    var done = false

    while (!done)
    {
      if (li == lend && ri == rend) done = true // both empty
      else if (li < lend && ri < rend) // both non-empty
      {
        val lv = l.items(li)
        val rv = r.items(ri)

        if (lv > rv)
        {
          out(i) = rv
          ri += 1
          i += 1
          invs += lend - li
        }
        else
        {
          assert(lv <= rv)
          out(i) = lv
          li += 1
          i += 1
        }
      }
      else if (li < lend) // right empty
      {
        assert(ri == rend)
        while (li < lend)
        {
          out(i) = l.items(li)
          li += 1
          i += 1
        }

        assert(li == lend)
        done = true

      }
      else // left empty
      {
        assert(li == lend)
        assert(ri < rend)


        while (ri < rend)
        {
          out(i) = r.items(ri)
          ri += 1
          i += 1
        }

        assert(ri == rend)
        done = true
      }
    }

    Result(out, l.invs + r.invs + invs)
  }

  def inversions(a: Array[Int]): Result =
  {
    val empty = Result(Array[Int](), 0)

    def core(start: Int, end: Int): Result =
    {
      if (start < end)
      {
        val mid = start + (end - start) / 2

        val l = core(start, mid)
        val r = core(mid + 1, end)
        val out = merge(l, r)
        out
      }
      else if (start == end) Result(Array(a(start)), 0)
      else empty

    }

    val out = core(0, a.length - 1)
    out
  }


  /*
    case class ResultList(items: List[Int], invs: Int)
    {
      def append(i: Int, moreInvs: Int) = ResultList(items ::: List(i), invs+moreInvs)
      def tail = ResultList(items.tail, invs)
      def merge(that: ResultList) = ResultList(items ::: that.items, invs + that.invs)
    }

    object EmptyResultList extends ResultList(List[Int](), 0)

    @tailrec
    final def mergeList(l: ResultList, r: ResultList, out: ResultList): ResultList =
    {
      if (l.items.isEmpty && r.items.isEmpty) out.merge(l).merge(r)
      else if (!l.items.isEmpty  && !r.items.isEmpty)
      {
        if (l.items.head == r.items.head) mergeList(l.tail, r.tail, out.append(l.items.head, 0).append(r.items.head, l.items.length-1))
        else if (l.items.head < r.items.head) mergeList(l.tail, r, out.append(l.items.head, 0))
        else
        {
          assert(l.items.head > r.items.head)
          mergeList(l, r.tail, out.append(r.items.head, l.items.length))
        }
      }
      else if (!l.items.isEmpty)
        {
          out.merge(l).merge(r)
        }
      else
      {
        assert(!r.items.isEmpty)
        out.merge(r).merge(l)
      }
    }

    def inversionsList(a: Array[Int]): ResultList =
    {

      def core(start: Int, end: Int): ResultList =
      {
        if (start < end)
        {
          val mid = start + (end - start) / 2

          val l = core(start, mid)
          val r = core(mid+1, end)
          val out = mergeList(l, r, EmptyResultList)
          out
        }
        else if (start == end) EmptyResultList.append(a(start), 0)
        else EmptyResultList

      }

      val out = core(0, a.length-1)
      out
    }
  */

  @tailrec
  def mergeList2(l: List[Int], r: List[Int], out: List[Int], invs: Long): (List[Int], Long) =
  {
    (l, r) match
    {
      case (Nil, Nil) => (out.reverse, invs)
      case (Nil, rh :: rt) => mergeList2(l, rt, rh :: out, invs)
      case (lh :: lt, Nil) => mergeList2(lt, r, lh :: out, invs)
      case (lh :: lt, rh :: rt) =>
        if (lh <= rh) mergeList2(lt, r, lh :: out, invs)
        else mergeList2(l, rt, rh :: out, invs + l.length)
    }
  }


  def inversionsList2(a: Array[Int]): (List[Int], Long) =
  {

    def core(start: Int, end: Int): (List[Int], Long) =
    {
      if (start < end)
      {
        val mid = start + (end - start) / 2

        val l = core(start, mid)
        val r = core(mid+1, end)
        val out = mergeList2(l._1, r._1, Nil, l._2 + r._2)
        out
      }
      else if (start == end) (List(a(start)), 0)
      else (Nil,0)

    }

    val out = core(0, a.length-1)
    out
  }

  def time[R](block: => R): R = {
    val t0 = System.nanoTime()
    val result = block    // call-by-name
    val t1 = System.nanoTime()
    println("Elapsed time: " + (t1 - t0)/1e6.toInt + "ms")
    result
  }

  def main2(args: Array[String]): Unit =
  {
    val a = new Array[Int](1e5.toInt)
    for (i <- 0 to a.length-1) a(i) = a.length-i

    println("List")
    for (i <- 1 to 3) time( inversionsList2(a) )

    println("Array")
    for (i <- 1 to 3) { val a2 = a.clone; time( inversions(a2) ) }
  }

  def main(args: Array[String]): Unit =
  {
    val s = new Scanner(System.in)

    val n = s.nextInt
    val a = new Array[Int](n)
    for (i <- 0 to n - 1) a(i) = s.nextInt

    println(inversions(a).invs)

  }
}
