import java.util.Scanner

import scala.annotation.tailrec

object Inversions
{
  case class Result(items: List[Int], invs: Int)
  {
    def append(i: Int, moreInvs: Int) = Result(items ::: List(i), invs+moreInvs)
    def tail = Result(items.tail, invs)
    def merge(that: Result) = Result(items ::: that.items, invs + that.invs)
  }

  object EmptyResult extends Result(List[Int](), 0)

  @tailrec
  final def merge(l: Result, r: Result, out: Result): Result =
  {
    if (l.items.isEmpty && r.items.isEmpty) out.merge(l).merge(r)
    else if (!l.items.isEmpty  && !r.items.isEmpty)
    {
      if (l.items.head == r.items.head) merge(l.tail, r.tail, out.append(l.items.head, 0).append(r.items.head, l.items.length-1))
      else if (l.items.head < r.items.head) merge(l.tail, r, out.append(l.items.head, 0))
      else
      {
        assert(l.items.head > r.items.head)
        merge(l, r.tail, out.append(r.items.head, l.items.length))
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

  def inversions(a: Array[Int]): Result =
  {

    def core(start: Int, end: Int): Result =
    {
      if (start < end)
      {
        val mid = start + (end - start) / 2

        val l = core(start, mid)
        val r = core(mid+1, end)
        val out = merge(l, r, EmptyResult)
        out
      }
      else if (start == end) EmptyResult.append(a(start), 0)
      else EmptyResult

    }

    val out = core(0, a.length-1)
    out
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
