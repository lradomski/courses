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

  def isBinarySearch(): Boolean =
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


  def isBalanced(): Boolean =
  {
    false

  }

  def isHeap: Boolean =
  {
    def isHeapCore(branch: Node[T]): Boolean = if (null != branch) this.key >= branch.key && branch.isHeap else true

    isHeapCore(left) && isHeapCore(right)
  }
}

object Node
{
  def fromInorder[T <% Ordered[T]](keys: List[T]): Node[T] =
  {
    def build(keys: List[T], level: Int): (Node[T], List[T]) =
    {
      def tryBuildLeft(keys: List[T], level: Int) : (Node[T], List[T]) = build(keys, level-1)

      def tryBuildParent(leftKeys: (Node[T],List[T])) : (Node[T], List[T]) =
      {
        if (null == leftKeys._1 || leftKeys._2.isEmpty) leftKeys
        else
        {
          val parent = new Node(leftKeys._1, null, null, leftKeys._2.head)
          leftKeys._1.parent = parent
          (parent, leftKeys._2.tail)
        }
      }

      def tryBuildRight(parentKeys: (Node[T],List[T]), level: Int) : (Node[T], List[T]) =
      {
        if (null == parentKeys._1 || parentKeys._2.isEmpty) parentKeys
        else
        {
          val (right, keysRight) = build(parentKeys._2, level-1)
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
