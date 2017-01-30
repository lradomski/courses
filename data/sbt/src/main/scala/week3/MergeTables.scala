import java.io.File
import java.util.Scanner

object MergeTables
{
  def main(args: Array[String]): Unit =
  {
    // /Users/luke/git/courses/data/download/Programming-Assignment-2/merging_tables/tests/116
    val s = if (args.isEmpty) new Scanner(System.in) else new Scanner(new File(args(0)))
    val s2 = if (args.isEmpty) null else new Scanner(new File(args(0) + ".a"))

    val n = s.nextInt
    val m = s.nextInt

    val counts = (for (i <- 1 to n) yield s.nextInt).toArray
    val parents = (for (i <- 1 to n) yield i - 1).toArray
    val ranks = (for (i <- 1 to n) yield 1).toArray

    var max = counts.max

    def parent(in: Int) =
    {
      var t = in
      while (t != parents(t)) t = parents(t)

      parents(in) = t
      t
    }

    for (i <- 1 to m)
    {
      val dest = s.nextInt - 1
      var src = s.nextInt - 1

      val psrc = parent(src)
      val pdest = parent(dest)

      if (pdest != psrc)
      {
        assert(counts(pdest) >= 0)
        assert(counts(psrc) >= 0)
        assert(ranks(pdest) > 0)
        assert(ranks(psrc) > 0)

        val newCount = counts(pdest) + counts(psrc)
        if (newCount > max) max = newCount


        if (ranks(pdest) >= ranks(psrc))
        {
          counts(pdest) = newCount
          counts(psrc) = -1
          parents(psrc) = pdest


          if (ranks(pdest) == ranks(psrc)) ranks(pdest) += 1
          ranks(psrc) = 0
        }
        else
        {
          counts(psrc) = newCount
          counts(pdest) = -1
          parents(pdest) = psrc

          ranks(pdest) = 0
        }


        println(max)
        if (s2 != null)
        {
          val aMax = s2.nextInt
          assert(max == aMax, "line " + (i + 1) + ": " + max + " != " + aMax)
        }


      }

    }
  }

}
