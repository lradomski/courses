import java.io.File
import java.util.Scanner

import scala.annotation.tailrec


object MakeHeap {

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

    //val vertices : Array[Int] = List(5, 4, 3, 2, 1).toArray

    val swaps = new scala.collection.mutable.Queue[(Int, Int)]()
    makeHeap(vertices, swaps)

    println(swaps.length)
    while (!swaps.isEmpty)
    {
      println(swaps.head._1 + " " + swaps.head._2)
      swaps.dequeue()
    }

//    if (args.isEmpty)
//    {
//      println(swaps.length)
//      while (!swaps.isEmpty)
//      {
//        println(swaps.head._1 + " " + swaps.head._2)
//        swaps.dequeue()
//      }
//    }
//    else
//    {
//      val s2 = new Scanner( new File(args(0)+".a"))
//
//      var n2 = s2.nextInt()
//
//      println(swaps.length)
//      assert(swaps.length == n2, "swaps.length == n2")
//
//      while (!swaps.isEmpty && n2 > 0 && s2.hasNext())
//      {
//
//        println(swaps.head._1 + " " + swaps.head._2)
//        val swap = swaps.dequeue()
//
//        val swap2 = (s2.nextInt(), s2.nextInt())
//        n2 -= 1
//        assert(swap == swap2, "swap == swap2")
//      }
//
//      assert(swaps.isEmpty, "swaps.isEmpty")
//      assert(n2 == 0, "n2 == 0")
//    }


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

  def makeHeap(a : Array[Int], swaps: scala.collection.mutable.Queue[(Int,Int)]) =
  {
    def hasParent(i: Int) = i > 0
    def hasLeft(i: Int) = left(i) < a.length
    def hasRight(i: Int) = right(i) < a.length

    def parent(i: Int) = (i - 1) / 2
    def left(i: Int) = 2 * i + 1
    def right(i: Int) = 2 * i + 2

    def isHeap(parent: Int, child: Int) = a(parent) <= a(child) // min-heap

    def p() =
    {
      a.foreach(i => print(i + ", ")); println();
    }


    def swap(i: Int, j: Int) =
    {
      //println(i + " <-> " + j)
      val store = a(j)
      a(j) = a(i)
      a(i) = store
      swaps.enqueue( (i,j) )
      //p()
    }

    //  def siftDown(i : Int) : Int =
    //  {
    //    println(">>> " + i)
    //    siftDownRecursive(i)
    //  }

    @tailrec
    def siftDown(i: Int) : Unit =
    {
      //assert(0 <= i && i < a.length, "0 <= i && i < a.length")
      //println(">> " + i)

      var p = i // new parent

      val r = right(i)
      if (r < a.length && !isHeap(p, r))
      {
        p = r
      }

      val l = left(i)
      if (l < a.length && !isHeap(p, l))
      {
        p = l
      }


      if (i != p)
      {
        swap(i, p)
        siftDown(p)
      }
    }

    for (i <- a.length / 2 - 1 to 0 by -1) siftDown(i)
  }



}
