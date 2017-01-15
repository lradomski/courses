import java.util.Scanner

object MaxExpr
{

  case class MinMax(min: Long, max: Long, isSet: Boolean)

  val ops = Map(
    '+' -> ((l: Long, r: Long) => l + r),
    '-' -> ((l: Long, r: Long) => l - r),
    '*' -> ((l: Long, r: Long) => l * r)
  )

  def calcOp(op: (Long, Long) => Long, l: MinMax, r: MinMax): MinMax =
  {
    val results = for
    {
      lv <- List(l.min, l.max)
      rv <- List(r.min, r.max)
    } yield op(lv, rv)

    MinMax(results.min, results.max, true)
  }


  def maxValue(s: String): Long =
  {
    val n = s.filter(_.isDigit).map(_.asDigit).toArray
    val o = s.filterNot(_.isDigit).map(ops(_)).toArray

    val exp = Array.ofDim[MinMax](n.length, n.length)

    def calcExp(i: Int, j: Int): MinMax =
    {
      def subExp(i: Int, j: Int) =
      {
        assert(exp(i)(j).isSet)
        exp(i)(j)
      }

      assert(0 <= i && i < n.length, "is not: 0 <= " + i + " < " + n.length)
      assert(i <= j && j < n.length, "is not: " + i + " <= " + j + " < " + n.length)

      if (i == j) MinMax(n(i), n(i), true)
      else if (i + 1 == j) calcOp(o(i), subExp(i, i), subExp(j, j))
      else
      {
        val res = for (k <- i until j) yield calcOp(o(k), calcExp(i,k), calcExp(k+1,j))
        MinMax(res.map(_.min).min, res.map(_.max).max, true)
      }

    }

    //for(i <- 0 until n.length) exp(i)(i) = MinMax(n(i), n(i))

    for
    {
      len <- 0 until n.length
      i <- 0 until n.length
      if (i + len < n.length)
    } exp(i)(i + len) = calcExp(i, i + len)

    assert(exp(n.length-1)(n.length-1).isSet)
    exp(0)(n.length-1).max
  }

  def main(args: Array[String]) =
  {
    val s = new Scanner(System.in)
    val expr = s.next
    val out = maxValue(expr)
    println(out)

  }
}
