import java.io.File
import java.util.Scanner

import scala.annotation.tailrec
import scala.collection.mutable

object TreeHeight {

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
    val vertices = readVertices(s, n)

    val out = treeHeight(vertices)

    if (args.isEmpty) println(out) else println(new Scanner( new File(args(0)+".a")).nextInt()==out );

    /*
    */
  }

  def readVertices(s: Scanner, n: Int): Array[Int] = {
    val tree = new Array[Int](n)
    for (i <- 0 to n - 1) {
      tree(i) = s.nextInt()
    }
    tree
  }

  def treeHeight(vertices: Array[Int]) : Int =
  {
//    def treeHeight(tree : Array[mutable.MutableList[Int]], root : Int) : Int =
//    {
//      val childNodes = tree(root)
//
//      var maxHeight = 0
//
//      for(child <- childNodes)
//        {
//          val childHeight = treeHeight(tree, child)
//          if (childHeight > maxHeight) maxHeight=childHeight
//        }
//
//      return maxHeight+1
//    }

    val tree = new Array[mutable.MutableList[Int]](vertices.length)

    var root = -1
    val roots = new mutable.Stack[Int]()
    val deepFirst = new mutable.Stack[Int]()
    val heights = new Array[Int](vertices.length)


    if (vertices.isEmpty) return 0

    for (i <- 0 to vertices.length-1) tree(i) = mutable.MutableList[Int]()

    for (i <- 0 to vertices.length-1)
    {
      heights(i) = -1

      val parent = vertices(i)

      if (parent == -1) { root = i; roots.push(i) }
      else tree(parent)+=i
    }

    while (!roots.isEmpty)
    {
      val root = roots.pop()
      deepFirst.push(root)

      val childNodes = tree(root)

      for(child <- childNodes)
        {
          roots.push(child)
        }
    }

    while(!deepFirst.isEmpty)
      {
        val root = deepFirst.pop()

        val childNodes = tree(root)

        var maxHeight = 0

        for(child <- childNodes)
          {
            val childHeight = heights(child)
            assert(0<=childHeight, "0<=childHeight")
            if (childHeight > maxHeight) maxHeight=childHeight
          }

        heights(root) = maxHeight+1
      }

    heights(root)
  }

}
