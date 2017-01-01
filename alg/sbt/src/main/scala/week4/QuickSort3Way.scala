import java.util.Scanner

object QuickSort3Way
{
  def qsort2way(a: Array[Int]): Array[Int] =
  {
    /*
  x ← A[ℓ] {pivot}
  j ← ℓ  // j = lo, hi = i, l = start, r = end
  for i from ℓ + 1 to r:
    if A[i] ≤ x:
      j ← j + 1
      swap A[j] and A[i]
      {A[ℓ + 1 . . . j] ≤ x, A[j + 1 . . . i] > x}
  swap A[ℓ] and A[j]
  return j
     */
    def core(start: Int, end: Int): Unit =
    {
      if (start < end)
      {
        val x = a(start)

        var lo = start

        for (hi <- start + 1 to end)
        {
          val key = a(hi)
          if (key <= x) // so it's a "lo" value
          {
            lo += 1
            a(hi) = a(lo)
            a(lo) = key
          }
        }

        a(start) = a(lo)
        a(lo) = x

        core(start, lo - 1)
        core(lo + 1, end)
      }


    }

    core(0, a.length - 1)
    a
  }

  def qsort3way(a: Array[Int]): Array[Int] =
  {
    /*
  x ← A[ℓ] {pivot}
  j ← ℓ  // j = lo, hi = i, l = start, r = end
  for i from ℓ + 1 to r:
    if A[i] ≤ x:
      j ← j + 1
      swap A[j] and A[i]
      {A[ℓ + 1 . . . j] ≤ x, A[j + 1 . . . i] > x}
  swap A[ℓ] and A[j]
  return j
     */
    def core(start: Int, end: Int): Unit =
    {
      if (start < end)
      {
        val x = a(start)

        var lo = start
        var same = start

        for (hi <- start + 1 to end)
        {
          val key = a(hi)
          if (key == x) // so it's a "same" value
          {
            same += 1
            a(hi) = a(same)
            a(same) = key
          }
          else if (key < x) // so it's a "lo" value
          {
            lo += 1
            same += 1
            a(hi) = a(same)
            a(same) = a(lo)
            a(lo) = key
          }
        }

        a(start) = a(lo)
        a(lo) = x

        core(start, lo - 1)
        core(same + 1, end)
      }


    }

    core(0, a.length - 1)
    a
  }

  def main(args: Array[String]): Unit =
  {
    val s = new Scanner(System.in)

    val n = s.nextInt
    val a = new Array[Int](n)
    for (i <- 0 to n - 1) a(i) = s.nextInt

    val as = qsort3way(a)
    as.foreach(e => print(e + " "))
    println
  }

}
