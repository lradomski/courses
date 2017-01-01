
object Utils
{
  def toStr(a: Array[Int]): String = a.foldLeft("")((s,i) => s + i + ",")


  def verify(a: Array[Int]): Unit =
  {
    verify(a, a.sorted)
  }

  def verify(a: Array[Int], as: Array[Int]): Unit =
  {
    assert(a.length == as.length)
    for (i <- 0 to a.length - 1) assert(a(i) == as(i), "/ [" + i + "]: " + a(i) + "!=" + as(i) + " // (" + toStr(a) + ")")
  }

  def verify(l: List[Int], ls: List[Int]): Unit = verify(l.toArray, ls.toArray)
}
