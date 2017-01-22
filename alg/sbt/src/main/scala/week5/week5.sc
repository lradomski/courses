import scala.collection.SortedSet
1 to 1
case class Point(x: Int, y: Int)
Set( Point(0,0), Point(0,0))
val points = Array[Int](1) //1,2,3,3,5,7,9,10)
def find(x: Int, l: Int, r: Int, sameOnRight: Boolean): Int =
{

  if (l >= r) l
  else
  {
    val m = (l + r) / 2
    if (if (sameOnRight) x <= points(m) else x < points(m)) find(x, l, m, sameOnRight)
    else find(x, m + 1, r, sameOnRight)
  }
}

val l = find(5, 0, points.length-1, true)
val r = find(5, 0, points.length-1, false)
points(l)
points(r)

//val a = Array.ofDim[Boolean](2,2)
//a(1)(1)
//1 until 3
//"1+5".filterNot(_.isDigit)//.map(_.asDigit)
//SortedSet(3, 1, 2, 2).toArray