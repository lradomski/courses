package tree

import scala.annotation.tailrec
import scala.math.Ordering

trait Tree[T]
{
  //  def isBinary: Boolean
  //  def isBalanced: Boolean
  def toSortedList: List[T]

  //  def find(key: T): Tree[T]
  //  def next(key: T): Tree[T]
  //  def search(low: T, high: T) : List[Tree[T]]
  //  def insert(key: T): Tree[T]
  //  def delete(key: T): Unit
}

class Node[T <% Ordered[T]](var left: Node[T], var right: Node[T], var parent: Node[T], var key: T) extends Tree[T]
{
  def this(key: T) = this(null, null, null, key)

  // root
  def this(parent: Node[T], key: T) = this(null, null, parent, key) // leaf

  override def toString: String =
  {
    def toString(t: Node[T], indent: Int): String =
    {
      if (null == t) "[]"
      else
      {
        val s = " " * indent

        val ret = "\n" + s + "[ " + t.key + "\n" +
          s + "  <<<" + toString(t.left, indent + 5) + "\n" +
          s + "  >>>" + toString(t.right, indent + 5) + "\n" +
          s + "]\n"

        ret
      }
    }

    toString(this, 0)
  }

  def lleaf(key: T): Node[T] =
  {
    this.left = new Node[T](this, key)
    this.left
  }

  def rleaf(key: T): Node[T] =
  {
    this.right = new Node[T](this, key)
    this.right
  }

  //  def isBinary: Boolean

  override def toSortedList: List[T] =
  {
    def core(tree: Node[T]): List[T] =
    {
      if (null == tree) List[T]()
      else
      {
        core(tree.left) ::: List(tree.key) ::: core(tree.right)
      }
    }

    core(this).sorted
  }

  private def ifElse[R](c: Boolean, onTrue: () => R, onFalse: () => R): R =
  {
    if (c) onTrue() else onFalse()
  }

  def inorder(): List[T] =
  {
    val l = ifElse(left == null, () => List[T](), () => left.inorder())
    val r = ifElse(right == null, () => List[T](), () => right.inorder())

    l ::: List(key) ::: r
  }

  def preorder(): List[T] =
  {
    val l = ifElse(left == null, () => List[T](), () => left.preorder())
    val r = ifElse(right == null, () => List[T](), () => right.preorder())

    List(key) ::: l ::: r
  }

  def postorder(): List[T] =
  {
    val l = ifElse(left == null, () => List[T](), () => left.preorder())
    val r = ifElse(right == null, () => List[T](), () => right.preorder())

    l ::: r ::: List(key)
  }

  //  def isBinarySearch: Boolean =
  //  {
  //    def isBinarySearch(child: Node[T], parent: Node[T], invariant: Node[T] => Boolean): Boolean =
  //    {
  //      if (child == null) true
  //      else if invariant(child)
  //    }
  //
  //    isBinarySearch(left, parent, child => child.key < parent.key)  && isBinarySearch(right, parent, child => parent.key < child.key)
  //
  //
  //  }
  def isBinarySearch: Boolean =
  {
    def core(node: Node[T], min: Node[T], max: Node[T]): Boolean =
    {
      def checkMin(min: Node[T], node: Node[T]): Boolean =
      {
        if (null == node || null == min) true
        else min.key < node.key
      }

      def checkMax(node: Node[T], max: Node[T]): Boolean =
      {
        if (null == max || null == node) true
        else node.key < max.key
      }

      if (node == null) true
      else checkMin(min, node) && checkMax(node, max) && core(node.left, min, node) && core(node.right, node, max)
    }

    core(this, null, null)
  }

  def isBinarySearch2: Boolean =
  {
    class Range(val min: Node[T], val max: Node[T], val isGood: Boolean)
    {
      def this() = this(null, null, true)

      def this(node: Node[T]) = this(node, node, true)

      def isEmpty = null == min && null == max

      def <(node: Node[T]): Boolean =
      {
        if (null == min && null == max) true
        else if (min == max) max.key < node.key
        else min.key < max.key && max.key < node.key
      }

      def >(node: Node[T]): Boolean =
      {
        if (null == min && null == max) true
        else if (min == max) node.key > min.key
        else node.key < min.key && min.key < max.key
      }

    }

    def range(node: Node[T]): Range =
    {
      if (null == node) new Range()
      else
      {
        val l: Range = range(node.left)
        if (l.isGood)
        {
          if (l < node)
          {
            val r = range(node.right)
            if (r.isGood)
            {
              if (r > node) new Range(l.min, r.max, true)
              else new Range(node, r.min, false)
            }
            else r
          }
          else new Range(l.max, node, false)

        }
        else l
      }
    }

    range(this).isGood
  }

  def isHeap: Boolean =
  {
    def isHeapCore(branch: Node[T]): Boolean = if (null != branch) this.key >= branch.key && branch.isHeap else true

    isHeapCore(left) && isHeapCore(right)
  }

  def isComplete: Boolean =
  {
    def core(node: Node[T]): Boolean =
    {
      if (node == null) true
      else
      {
        core(node.left) && core(node.right) && ((null == node.left) == (null == node.right))
      }
    }

    core(this)
  }

  def isFull: Boolean =
  {
    def core(nodes: List[Node[T]], fullNode: Boolean): Boolean =
    {
      if (nodes.isEmpty) true
      else
      {
        val node = nodes.head
        if (fullNode)
        {
          if (null != node.left && null != node.right) core(nodes.tail ::: List(node.left, node.right), fullNode)
          else if (null != node.left) core(nodes.tail ::: List(node.left), false)
          else if (null != node.right) false
          else core(nodes.tail, false)
        }
        else
        {
          if (null == node.left && null == node.right) core(nodes.tail, false)
          else false
        }
      }
    }

    def coreIter: Boolean =
    {
      var nodes = List[Node[T]](this)
      var fullNode = true
      var ok = true

      while (!nodes.isEmpty && ok)
      {
        val node = nodes.head

        if (fullNode)
        {
          if (null != node.left && null != node.right) nodes = nodes.tail ::: List(node.left, node.right)
          else if (null != node.left)
          {
            nodes = nodes.tail ::: List(node.left)
            fullNode = false
          }
          else if (null != node.right) ok = false
          else
          {
            nodes = nodes.tail
            fullNode = false
          }
        }
        else
        {
          if (null == node.left && null == node.right) nodes = nodes.tail
          else ok = false
        }
      }

      ok
    }

    //core(List(this), true)
    coreIter
  }

  def isPerfect: Boolean =
  {
    var nodes = List[Node[T]](null, this)
    var ok = true
    var newLevel = false
    var invariant: Node[T] => Boolean = node => node.left != null && node.right != null

    while (!nodes.isEmpty && ok)
    {
      val node = nodes.head
      nodes = nodes.tail

      if (node == null) newLevel = true
      else
      {
        if (newLevel)
        {
          newLevel = false
          nodes = nodes ::: List[Node[T]](null)
          if (node.left == null && node.right == null) invariant = node => node.left == null && node.right == null
          else ok = invariant(node)
        }
        else ok = invariant(node)

        if (ok && node.left != null  && node.right != null) nodes = nodes ::: List(node.left, node.right)
      }
    }

    ok  && newLevel
  }


  def isPerfect2: Boolean =
  {
    var nodes = List[Node[T]](this)
    var ok = true
    var first = this
    var last = this
    var isFinal = false

    while (!nodes.isEmpty && ok)
    {
      val node = nodes.head
      nodes = nodes.tail

      val isEmpty = (node.left == null && node.right == null)
      val isFull = (node.left != null && node.right != null)

      if (isEmpty)
      {
        if (node == first) isFinal = true
        else ok = isFinal
      }
      else if (isFull)
      {
        if (node == first) first = node.left
        else if (node == last) last = node.right
        nodes = nodes ::: List(left, right)
      }
      else ok = false

    }

    ok && isFinal
  }


  def isBalanced: Boolean =
  {
    def height(node: Node[T]): Int =
    {
      if (null == node) 0
      else math.max(height(node.left), height(node.right)) + 1
    }

    def core(node: Node[T]): Boolean =
    {
      if (null == node) true
      else core(this.left) && core(this.right) && math.abs(height(node.left) - height(node.right)) <= 1
    }

    def core2(node: Node[T]): (Boolean, Int) = // isBalanced,height
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

}

object Node
{
  //  def fromInorder[T <% Ordered[T]](keys: List[T]): Node[T] =
  //  {
  //    buildLeft
  //
  //  }

  def fromInorder2[T <% Ordered[T]](keys: List[T]): Node[T] =
  {
    def build(keys: List[T], level: Int): (Node[T], List[T]) =
    {
      def tryBuildLeft(keys: List[T], level: Int): (Node[T], List[T]) = build(keys, level - 1)

      def tryBuildParent(leftKeys: (Node[T], List[T])): (Node[T], List[T]) =
      {
        if (null == leftKeys._1 || leftKeys._2.isEmpty) leftKeys
        else
        {
          val parent = new Node(leftKeys._1, null, null, leftKeys._2.head)
          leftKeys._1.parent = parent
          (parent, leftKeys._2.tail)
        }
      }

      def tryBuildRight(parentKeys: (Node[T], List[T]), level: Int): (Node[T], List[T]) =
      {
        if (null == parentKeys._1 || parentKeys._2.isEmpty) parentKeys
        else
        {
          val (right, keysRight) = build(parentKeys._2, level - 1)
          if (null != right)
          {
            parentKeys._1.right = right
            right.parent = parentKeys._1
          }

          (parentKeys._1, keysRight)
        }
      }

      if (level == 0)
      {
        if (keys.isEmpty) (null, keys)
        else (new Node(null, null, null, keys.head), keys.tail)
      }
      else
      {
        tryBuildRight(tryBuildParent(tryBuildLeft(keys, level)), level)
        //        val (left, keysLeft) = build(keys, level - 1)
        //        if (null == left || keysLeft.isEmpty) (left, keysLeft)
        //        else
        //        {
        //          val parent = new Node(left, null, null, keysLeft.head)
        //          val keysParent = keysLeft.tail
        //
        //          if (keysParent.isEmpty) (parent, keysParent)
        //          else
        //          {
        //            val (right, keysRight) = build(keysParent, level-1)
        //            if (null != right)
        //            {
        //              parent.right = right
        //              right.parent = parent
        //            }
        //
        //            (parent, keysRight)
        //          }
        //        }
      }
    }

    def buildUp(left: Node[T], level: Int, keys: List[T]): Node[T] =
    {
      assert(left != null, "left != null")
      assert(0 < level, "0 < level")

      if (keys.isEmpty) left
      else
      {
        val parent = new Node(left, null, null, keys.head)
        left.parent = parent

        val (right, keysRight) = build(keys.tail, level - 1)
        if (null == right) parent
        else
        {
          right.parent = parent
          parent.right = right
          buildUp(parent, level + 1, keysRight)
        }
      }
    }

    val (left, keysLeft) = build(keys, 0)
    if (null == left) left else buildUp(left, 1, keysLeft)
  }

}
