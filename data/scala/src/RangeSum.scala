import java.io.File
import java.util.Scanner



class Set
{
  class Node[T <% Ordered[T]](var left: Node[T], var right: Node[T], var parent: Node[T], var key: T)

  type Tree = Node[Int]

  var root: Tree = null
  var sum: Int = 0

  def x = sum

  val M = (1e9 + 1).toInt

  def hash(i: Int): Int = (i + x) % M

  def find(arg: Int): Tree =
  {
    val i = hash(arg)
    var n = root
    var done = false

    while (n != null && !done)
    {
      if (n.key == i) done = true
      else if (i < n.key) if (n.left == null) done = true else n = n.left
      else if (n.key < i) if (n.right == null) done = true else n = n.right
      else assert(false, "Impossible case")
    }

    n
  }

  def exists(arg: Int): Boolean =
  {
    val i = hash(arg)
    if (null != root) find(i).key == i else false
  }

  def add(arg: Int): Tree =
  {
    // TODO: splay
    val i = hash(arg)

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

  def del(arg: Int): Unit =
  {
    val i = hash(arg)
    val node = find(i)
    if (node.key == i)
    {
//        if (node.left == null && node.right == null)
    }

  }

  def next(node: Tree): Tree =
  {
    def leftDescendent(node: Tree): Tree =
    {
      var ret = node
      while (ret.left != null) ret = ret.left
      ret
    }

    def rightAncestor(node: Tree): Tree =
    {
      var ret = node
      while (ret.parent != null && ret.parent.key < ret.key) ret = ret.parent
      ret
    }

    if (node == null) null
    else
    {
      if (node.right != null) leftDescendent(node.right)
      else if (node.parent != null) rightAncestor(node.parent)
      else null
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


}

object RangeSum
{

  def main(args: Array[String]) =
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
