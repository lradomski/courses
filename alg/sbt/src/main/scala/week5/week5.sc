import scala.collection.SortedSet
case class Point(x: Int, y: Int)
Set( Point(0,0), Point(0,0))
val points = Array(1,2,3,3,7,9,10)
def find(x: Int, l: Int, r: Int, sameOnRight: Boolean): Int =
{

  if (l >= r) l
  else
  {
    //assert(points(l) <= x)
    //assert(x <= points(r))

    val m = (l + r) / 2
    if (if (sameOnRight) x <= points(m) else x < points(m)) find(x, l, m, sameOnRight)
    else find(x, m + 1, r, sameOnRight)
  }
}

find(5, 0, points.length-1, true)
find(5, 0, points.length-1, false)

//val a = Array.ofDim[Boolean](2,2)
//a(1)(1)
//1 until 3
//"1+5".filterNot(_.isDigit)//.map(_.asDigit)
//SortedSet(3, 1, 2, 2).toArray