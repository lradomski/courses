import java.util

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

val a = new Array[Int](5)
f(a)
println(a.length/2-1)

a.foreach(i => print(i + ", ")); println()

makeHeap(a, new mutable.Queue[(Int, Int)]())

a.foreach(i => print(i + ", ")); println()

def f(a : Array[Int]) =
{
  for (i <- 0 to a.length-1)
    {
      a(i) = a.length-i
    }
}

def makeHeap(a : Array[Int], swaps: mutable.Queue[(Int,Int)]) =
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
  println(swaps.length)
  while (!swaps.isEmpty)
    {
      println(swaps.head._1 + " " + swaps.head._2)
      swaps.dequeue()
    }
}
