import java.io.File
import java.util.Scanner

import scala.annotation.tailrec

//class Node[T <% Ordered[T]](var left: Node[T], var right: Node[T], var parent: Node[T], var key: T)

class Tree(var left: Tree, var right: Tree, var parent: Tree, var key: Int)
{
  var height: Int = 0
  var treeSum: Long = 0

  def reset(): Unit =
  {
    left = null
    right = null
    parent = null
  }

  def isLeaf = left == null && right == null

  def isRoot = parent == null

  def adjustHeight: Unit =
  {
    val old = height
    val (lh, ls) = if (left != null) (left.height, left.treeSum) else (0, 0L)
    val (rh, rs) = if (right != null) (right.height, right.treeSum) else (0, 0L)
    height = 1 + math.max(lh, rh)
    treeSum = ls + rs
    treeSum += key

    if (old != height && null != parent) parent.adjustHeight
  }

  def isBalanced: Boolean =
  {
    def height(node: Tree): Int =
    {
      if (null == node) 0
      else math.max(height(node.left), height(node.right)) + 1
    }

    def core(node: Tree): Boolean =
    {
      if (null == node) true
      else core(this.left) && core(this.right) && math.abs(height(node.left) - height(node.right)) <= 1
    }

    def core2(node: Tree): (Boolean, Int) = // isBalanced,height
    {
      if (node == null) (true, 0)
      else
      {
        val l = core2(node.left)
        if (l._1)
        {
          val r = core2(node.right)
          val isBalanced = (r._1 && math.abs(l._2 - r._2) <= 1)
          val height = math.max(l._2, r._2) + 1
          (isBalanced, height)
        }
        else (false, -1) // height doesn't matter here
      }
    }

    //core(this)
    core2(this)._1
  }


  override def toString: String =
  {
    def toString(t: Tree, indent: Int): String =
    {
      if (null == t) "[]"
      else
      {
        val s = " " * indent

        val ok = "" //if (isBinarySearch) "" else " (?)"
      val sum = " (" + t.treeSum + ") "

        val ret = "\n" + s + "[ " + t.key + sum + ok + "\n" +
          s + "  <<<" + toString(t.left, indent + 5) + "\n" +
          s + "  >>>" + toString(t.right, indent + 5) + "\n" +
          s + "]\n"

        ret
      }
    }

    toString(this, 0)
  }
}

class Set
{
  //  class Tree(left: Node[Int], right: Node[Int], parent: Node[Int], key: Int) extends Node[Int](left, right, parent, key)
  //  {
  //    override def ownSum: Int = key
  //  }
  //  type Tree = Node[Int]

  var root: Tree = null
  var lastSum: Long = 0

  def get = root


  var debug = false

  val M = (1e9 + 1).toInt

  var check = List[Int]()

  override def toString: String =
  {
    if (null == root) "[]" else root.toString
  }

  def hash(i: Int): Int =
  {
    def x = lastSum

    val ret: Long = (i + x) % M
    //println("// " + i + " --[" + x + "]--> " + ret)
    assert(ret < Int.MaxValue)
    ret.toInt
  }

  def format(i: Long): String = i.toString

  //java.text.NumberFormat.getIntegerInstance.format(i) // i.toString

  def height = if (root != null) root.height else 0

  def isBalanced = if (root != null) root.isBalanced else true

//  @tailrec
//  final def adjustSumToTop(n: Tree): Unit =
//  {
//    if (n != null)
//    {
//      n.adjustHeight
//      adjustSumToTop(n.parent)
//    }
//  }

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


  def rebalance(node: Tree): Tree =
  {
    def updateParent(in: Tree, out: Tree, parent: Tree): Unit =
    {
      if (null != parent)
      {
        if (parent.left == out) parent.left = in
        else if (parent.right == out) parent.right = in
        else assert(false, "reparent: parent -> in")
      }
      else
      {
        root = in
      }
    }


    def rotateRight(out: Tree): Tree = // A <- *B -> C   ==>   A -> *B -> C
    {
      assert(out != null)
      assert(out.left != null)
      val parent = out.parent
      val in = out.left


      if (in.right != null) in.right.parent = out
      out.left = in.right

      out.parent = in
      in.right = out

      in.parent = parent
      updateParent(in, out, parent)
      out.adjustHeight
      in
    }

    def rotateLeft(out: Tree): Tree = // A <- *B -> C   ==>   A <- *B <- C
    {
      assert(out != null)
      assert(out.right != null)
      val parent = out.parent
      val in = out.right


      if (in.left != null) in.left.parent = out
      out.right = in.left

      out.parent = in
      in.left = out

      in.parent = parent
      updateParent(in, out, parent)
      out.adjustHeight
      in
    }

    def h(node: Tree) = if (node != null) node.height else 0

    def rebalanceRight(node: Tree): Tree =
    {
      assert(node != null)
      assert(node.left != null)

      if (h(node.left.right) > h(node.left.left)) rotateLeft(node.left)
      rotateRight(node)
    }

    def rebalanceLeft(node: Tree): Tree =
    {
      assert(node != null)
      assert(node.right != null)

      if (h(node.right.left) > h(node.right.right)) rotateRight(node.right)
      rotateLeft(node)
    }


    if (null != node)
    {
      val parent = node.parent
      val out =
        if (h(node.left) > h(node.right) + 1) rebalanceRight(node)
        else if (h(node.left) + 1 < h(node.right)) rebalanceLeft(node)
        else
        {
          node.adjustHeight
          node

        }
      rebalance(parent)
      out
    }
    else null
  }


  def add(i: Int): Tree =
  {
    assert(i < M, "add: hash")

    def dbgAdd: Unit =
    {
      println("// s.add(" + format(i) + ")")
      if (!check.exists(key => key == i)) check = check ::: List(i)
    }

    if (debug) dbgAdd

    val ret =
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

    rebalance(ret)
    ret
  }

  def del(i: Int): Boolean =
  {
    assert(i < M, "del: hash")
    val node = find(i)
    if (null != node && node.key == i)
    {
      del(node)
      true
    }
    else false
  }

  def del(node: Tree): Unit =
  {

    def dbgDel(i: Int): Unit =
    {
      println("// s.del(" + format(i) + ")")
      check = check.filter(key => key != i)
    }

    def promote(toPromote: Tree, toSkip: Tree): Tree =
    {
      assert(toSkip != null, "promote: toSkip")
      assert(if (toPromote != null) toPromote.parent == toSkip else true, "promote: toPromote")

      val newParent = toSkip.parent

      if (newParent != null)
      {
        if (toSkip == newParent.left) newParent.left = toPromote
        else if (toSkip == newParent.right) newParent.right = toPromote
        else assert(false, "promote: newParent->toSkip")
        newParent.adjustHeight
      }
      else
      {
        root = toPromote
      }

      if (toPromote != null) toPromote.parent = newParent
      newParent
    }

    def replaceWith(old: Tree, newNode: Tree): Unit =
    {
      old.key = newNode.key
      newNode.reset
    }

    if (node != null)
    {
      if (debug) dbgDel(node.key)

      val nextNode = next(node)
      if (nextNode == null)
      {
        assert(node.right == null)
        val parent = promote(node.left, node)
        rebalance(parent)
      }
      else
      {
        if (nextNode.left == null)
        {
          val parent = promote(nextNode.right, nextNode)
          replaceWith(node, nextNode)
          rebalance(parent)

        }
        else
        {
          assert(node.right == null)
          val parent = promote(node.left, node)
          rebalance(parent)
        }
      }

      //node.reset
    }
  }

  def mergeWithRoot(l: Tree, r: Tree, parent: Tree): Tree =
  {
    def h(n: Tree) = if (n != null) n.height else 0

    assert(parent != null)
    assert(if (l != null) l.key < parent.key else true)
    assert(if (r != null) parent.key < r.key else true)

    parent.left = l
    parent.right = r
    if (l != null) l.parent = parent
    if (r != null) r.parent = parent
    parent.parent = null
    parent.adjustHeight

    parent
  }

  def mergeWithRootAVL(l: Tree, r: Tree, parent: Tree): Tree =
  {
    def h(n: Tree) = if (n != null) n.height else 0

    assert(parent != null)
    assert(if (l != null) l.key < parent.key else true)
    assert(if (r != null) parent.key < r.key else true)

    if (math.abs(h(l) - h(r)) <= 1)
    {
      mergeWithRoot(l, r, parent)
    }
    else if (h(l) > h(r))
    {
      assert(l != null)
      val newRight = mergeWithRootAVL(l.right, r, parent)
      l.right = newRight
      if (newRight != null) newRight.parent = l
      l.parent = null
      val out = rebalance(l)
      l.adjustHeight
      out.adjustHeight //?
      out
    }
    else
    {
      assert(h(l) < h(r))
      assert(r != null)

      val newLeft = mergeWithRootAVL(l, r.left, parent)
      r.left = newLeft
      if (newLeft != null) newLeft.parent = r
      r.parent = null
      val out = rebalance(r)
      r.adjustHeight
      out.adjustHeight //?
      out
    }


  }


  def split(x: Int): (Set, Set) =
  {

    def splitCore(node: Tree, x: Int): (Tree, Tree) = // l.key <= x , x < r.key
    {
      if (node == null) (null, null)
      else
      {
        if (node.key <= x)
        {
          // node.left tree already meets condition entirely
          // node.right may still have some nodes which we need on the left
          val (splitL, outR) = splitCore(node.right, x)

          // merge "left" nodes retrieved from right side with node.left under node
          val outL = mergeWithRoot(node.left, splitL, node)
          (outL, outR)
        }
        else // x < node.key
        {
          // symmetrical to above
          // node.right tree already meets condition entirely
          // node.left may still have some nodes which we need on the right
          val (outL, splitR) = splitCore(node.left, x)

          // merge "right" nodes retrieved from left side with node.right under node
          val outR = mergeWithRoot(splitR, node.right, node)
          (outL, outR)
        }
      }
    }

    val (left, right) = splitCore(root, x)
    if (left != null) left.parent = null
    if (right != null) right.parent = null

    root = null
    val setLeft = new Set;
    setLeft.root = left
    val setRight = new Set;
    setRight.root = right
    (setLeft, setRight)
  }

  def max: Tree =
  {
    def findMax(node: Tree): Tree =
    {
      assert(node != null)
      if (null != node.right) findMax(node.right)
      else node
    }

    if (null != root) findMax(root) else null
  }

  def merge(left: Set, right: Set): Set =
  {
    assert(root == null)
    assert(left != null)
    assert(right != null)

    if (left.root != null)
    {
      val parent = left.max
      left.del(parent)
      root = mergeWithRootAVL(left.root, right.root, parent)
    }
    else
    {
      root = right.root
    }

    left.root = null
    right.root = null
    this
  }


  def sum(l: Int, r: Int): Long = sumSplitMerge(l, r) // sumNextLoop(l,r) //

  def sumSplitMerge(l: Int, r: Int): Long =
  {
    def addUp(node: Tree): Long =
    {
      if (null == node) 0
      else addUp(node.left) + addUp(node.right) + node.key
    }

    assert(l < M, "sum: hash")
    assert(r < M, "sum: hash")
    assert(l <= r, "sum: " + l + " > " + r)

    val s = split(l - 1)
    val (range, ignore) = s._2.split(r)

    val total = if (range.root != null) range.root.treeSum else 0
    //addUp(range.root)

    if (debug)
    {
      println(
        "// s.sum(" + format(l) + ", " + format(r) + ") // = " + format(total) +
          " // " + format(check.foldLeft(0L)((sum, key) => if (l <= key && key <= r) sum + key else sum)) +
          " // " + check.filter(key => (l <= key && key <= r)))
    }

    merge(s._1, (new Set).merge(range, ignore))

    total
  }

  def sumNextLoop(l: Int, r: Int): Long =
  {
    assert(l < M, "sum: hash")
    assert(r < M, "sum: hash")
    assert(l <= r, "sum: " + l + " > " + r)

    var sum: Long = 0
    var node = find(l)

    while (null != node && node.key <= r)
    {
      if (l <= node.key && node.key <= r) sum += node.key
      node = next(node)
    }

    if (debug)
    {
      println(
        "// s.s(" + format(l) + ", " + format(r) + ") // = " + format(sum) +
          " // " + format(check.foldLeft(0L)((sum, key) => if (l <= key && key <= r) sum + key else sum)) +
          " // " + check.filter(key => (l <= key && key <= r)))
    }

    sum
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

  def findNext(i: Int): Tree = next(find(i))

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
    val ret = if (l <= r) sum(l, r) else sum(r, l)
    lastSum = ret
    ret
  }

}

object RangeSumTest
{
  def main(args: Array[String]): Unit = RangeSum.mainAll(args)
}

object RangeSum
{
  def mainAll(args: Array[String]): Unit =
  {
    val path = "/Users/luke/git/courses/data/download/Programming-Assignment-4/set_range_sum/tests/"
    val files = List("01", "04", "05", "20", "36", "83")

    files.foreach(file =>
    {
      println(" =======> " + file)
      mainCore(List(path + file).toArray)
    })
  }

  def main(args: Array[String]): Unit = mainCore(args)

  def mainCore(args: Array[String]): Unit =
  {
    case class Op(op: String, left: Int, right: Int)

    val s: Scanner = if (args.isEmpty) new Scanner(System.in) else new Scanner(new File(args(0)))
    val sa = if (args.isEmpty) null else new Scanner(new File(args(0) + ".a"))
    var opCount = 0
    var outCount = 0

    def verify(s: String): String =
    {
      if (null != sa)
      {
        val out = sa.nextLine()
        assert(out == s, "op:" + (opCount + 2) + "/line:" + (outCount + 1) + "> [" + s + "] != [" + out + "]")
        outCount += 1
      }
      s
    }

    val n = s.nextInt()

    val set = new Set
    //set.debug = true
    for (i <- 0 to n - 1)
    {
      opCount = i
      val op = s.next()
      if (op == "+") set + s.nextInt
      else if (op == "-") set - s.nextInt
      else if (op == "?") if (set.debug) verify(set ? s.nextInt) else println(verify(set ? s.nextInt)) //
      else if (op == "s") if (set.debug) verify(set.s(s.nextInt, s.nextInt).toString) else println(verify(set.s(s.nextInt, s.nextInt).toString)) //
    }

    if (!args.isEmpty)
    {
      val sa = new Scanner(new File(args(0)))
    }
  }
}
