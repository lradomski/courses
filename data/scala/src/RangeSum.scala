import java.io.File
import java.util.Scanner

import scala.annotation.tailrec


class Set
{

  class Node[T <% Ordered[T]](var left: Node[T], var right: Node[T], var parent: Node[T], var key: T)
  {
    def reset(): Unit =
    {
      left = null
      right = null
      parent = null
    }

    override def toString: String =
    {
      def toString(t: Node[T], indent: Int): String =
      {
        if (null == t) "[]"
        else
        {
          val s = " " * indent

          val ok = "" //if (isBinarySearch) "" else " (?)"

          val ret = "\n" + s + "[ " + t.key + ok + "\n" +
            s + "  <<<" + toString(t.left, indent + 5) + "\n" +
            s + "  >>>" + toString(t.right, indent + 5) + "\n" +
            s + "]\n"

          ret
        }
      }

      toString(this, 0)
    }
  }

  type Tree = Node[Int]

  var root: Tree = null
  var lastSum: Long = 0

  def x = lastSum

  val M = (1e9 + 1).toInt

  override def toString: String =
  {
    def toString(t: Tree, indent: Int): String =
    {
      if (null == t) "[]"
      else
      {
        val s = " " * indent

        val ok = if (isBinarySearch) "" else " (?)"

        val ret = "\n" + s + "[ " + t.key + ok + "\n" +
          s + "  <<<" + toString(t.left, indent + 5) + "\n" +
          s + "  >>>" + toString(t.right, indent + 5) + "\n" +
          s + "]\n"

        ret
      }
    }

    toString(root, 0)
  }

  def hash(i: Int): Int =
  {
    val ret = ((i + x) % M).toInt
    println(i + " --[" + x + "]--> " + ret)
    ret
  }

  def find(i: Int): Tree =
  {
    assert(i < M, "find: hash")
    var n = root
    var done = false

    while (n != null && !done)
    {
      if (n.key == i) done = true
      else if (i < n.key) if (n.left == null) done = true else n = n.left
      else if (n.key < i) if (n.right == null) done = true else n = n.right
      else assert(false, "find: impossible")
    }

    n
  }

  def exists(i: Int): Boolean =
  {
    assert(i < M, "exists: hash")
    if (null != root) find(i).key == i else false
  }

  def add(i: Int): Tree =
  {
    assert(i < M, "add: hash")
    // TODO: splay

    if (null == root)
    {
      root = new Tree(null, null, null, i)
      root
    }
    else
    {
      val parent = find(i)
      if (i < parent.key)
      {
        assert(parent.left == null, "add: parent.left")
        parent.left = new Tree(null, null, parent, i)
        parent.left
      }
      else if (parent.key < i)
      {
        assert(parent.right == null, "add: parent.right")
        parent.right = new Tree(null, null, parent, i)
        parent.right
      }
      else
      {
        assert(parent.key == i, "add: parent.key")
        parent
      }
    }
  }

  def del(i: Int): Unit =
  {
    assert(i < M, "del: hash")

    def promote(node: Tree): Unit =
    {
      if (node != null)
      {
        if (node.parent != null)
        {
          if (node.parent.parent != null)
          {
            val newParent = node.parent.parent
            if (node.key < newParent.key) newParent.left = node
            else if (newParent.key < node.key) newParent.right = node
            else assert(false, "promote: same key")
            node.parent = newParent
          }
          else
          {
            root = node
            node.parent = null
          }
        }
        else assert(false, "promote: no parent")
      }
    }

    def replaceWith(old: Tree, newNode: Tree): Unit =
    {
      old.key = newNode.key
      newNode.reset
      // TODO: actually re-attach the node, not just transfer key
    }

    val node = find(i)
    if (node.key == i)
    {
      val nextNode = next(node)
      if (nextNode == null)
      {
        assert(node.right == null)
        promote(node.left)
      }
      else
      {
        if (nextNode.left == null)
        {
          promote(nextNode.right)
          replaceWith(node, nextNode)
        }
        else
        {
          assert(node.right == null)
          promote(node.left)
        }
      }
    }
  }

  def sum(l: Int, r: Int): Long =
  {
    assert(l < M, "sum: hash")
    assert(r < M, "sum: hash")

    var sum = 0
    var node = find(l)

    while (null != node && l <= node.key && node.key <= r)
    {
      sum += node.key
      node = next(node)
    }

    lastSum = sum
    lastSum
  }


  def next(node: Tree): Tree =
  {
    @tailrec
    def leftDescendent(node: Tree): Tree =
    {
      if (node.left == null) node
      else leftDescendent(node.left)
    }

    @tailrec
    def rightAncestor(node: Tree): Tree =
    {
      if (node.parent == null) null
      else
      {
        if (node.parent.key > node.key) node.parent else rightAncestor(node.parent)

      }
    }

    if (node == null) null
    else
    {
      if (node.right != null) leftDescendent(node.right)
      else rightAncestor(node)
    }

  }

  def isBinarySearch: Boolean =
  {
    def core(node: Tree, min: Tree, max: Tree): Boolean =
    {
      def checkMin(min: Tree, node: Tree): Boolean =
      {
        if (null == node || null == min) true
        else min.key < node.key
      }

      def checkMax(node: Tree, max: Tree): Boolean =
      {
        if (null == max || null == node) true
        else node.key < max.key
      }

      if (node == null) true
      else checkMin(min, node) && checkMax(node, max) && core(node.left, min, node) && core(node.right, node, max)
    }

    core(root, null, null)
  }

  ////////////////////////////////
  def +(arg: Int): Set =
  {
    val i = hash(arg)
    add(i)
    this
  }

  def -(arg: Int): Set =
  {
    val i = hash(arg)
    del(i)
    this
  }

  def ?(arg: Int): String =
  {
    val i = hash(arg)
    if (exists(i)) "Found" else "Not found"
  }

  def s(larg: Int, rarg: Int) =
  {
    val l = hash(larg)
    val r = hash(rarg)
    sum(l, r)
  }

}

object RangeSum
{
  def main(args: Array[String]): Unit =
  {
    val s = new Set
    s + 1
    s + 2
    s.s(1, 2)
  }

  def main2(args: Array[String]): Unit =
  {
    case class Op(op: String, left: Long, right: Long)

    val s: Scanner = if (args.isEmpty) new Scanner(System.in) else new Scanner(new File(args(0)))

    val n = s.nextInt()

    for (i <- 0 to n - 1)
    {
      val op = Op(s.next(), s.nextLong, s.nextLong)

    }
  }
}
