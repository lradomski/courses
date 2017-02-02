import java.io.File
import java.util.Scanner

import sun.reflect.generics.reflectiveObjects.NotImplementedException

import scala.annotation.tailrec

object Rope
{

  case class CharOffset(var char: Char, var off: Int)
  {
    override def toString: String = char + "@" + off

    def absPos(pos: Int) = pos + off
  }

  case class OffsetRange(  left: Int, var right: Int)
  {
    assert(left <= right, "left<=right (" + left + "," + right + ")")

    def toAbs(pos: Int) = OffsetRange(pos + left, pos + right)

    def toRel(pos: Int) = OffsetRange(left - pos, right - pos)

    override def toString = "[" + left + ":" + right + "]"
  }

  //val EmptyRange = OffsetRange(Int.MaxValue, Int.MinValue)

  case class NodePos(n: Tree, var parentPos: Int)
  {
    def absPos = if (n != null) n.key.absPos(parentPos) else 0
    assert(absPos >= 0, "absPos<0 (" + absPos + ")")

    def isNull = n == null

    def isNotNull = n != null


    def hasParent = n != null && n.parent != null

    def hasNotParent = n != null && n.parent == null

    def isRoot = hasNotParent

    def parent = if (hasParent) NodePos(n.parent, parentPos - n.parent.key.off) else EmptyNodePos

    def setParent(newParent: NodePos): NodePos =
    {
      if (newParent.isNotNull)
      {
        n.key.off = absPos - newParent.absPos
        parentPos = newParent.absPos
        n.parent = newParent.n
      }
      else
      {
        n.key.off = absPos
        parentPos = 0
        n.parent = null
      }

      this
    }

    def left = NodePos(n.left, absPos)

    def hasLeft = n.left != null

    def hasNotLeft = n.left == null

    def setLeft(newChild: NodePos): NodePos =
    {
      n.left = newChild.n
      if (newChild.isNotNull)
      {
        assert(newChild.absPos < absPos)
//        val absRange = n.range.toAbs(absPos)
//        n.range = OffsetRange(newChild.n.range.toAbs(newChild.absPos).left, absRange.right).toRel(absPos)
      }
//      else
//      {
//        n.range = OffsetRange(0, n.range.right)
//      }
      this
    }

    def right = NodePos(n.right, absPos)

    def hasRight = n.right != null

    def hasNotRight = n.right == null

    def setRight(newChild: NodePos) =
    {
      n.right = newChild.n
      if (newChild.isNotNull)
      {
        assert(absPos < newChild.absPos)
//        val absRange = n.range.toAbs(absPos)
//        n.range = OffsetRange(absRange.left, newChild.n.range.toAbs(newChild.absPos).right).toRel(absPos)
      }
//      else
//      {
//        n.range = OffsetRange(n.range.left, 0)
//      }

      this
    }

    def adjustHeight = n.adjustHeight
  }

  val EmptyNodePos = NodePos(null, 0)


  class Tree(var left: Tree, var right: Tree, var parent: Tree, var key: CharOffset)
  {
    var height: Int = 1
    var range = OffsetRange(0, 0)

    def reset(): Unit =
    {
      left = null
      right = null
      parent = null
      height = 1
      val range = OffsetRange(key.off, key.off)
    }

    def withPos(pos: Int) = NodePos(this, pos)

    def isLeaf = left == null && right == null

    def isRoot = parent == null

    def adjustHeight: Unit =
    {
      val old = (height, range)

      // adjust child ranges so they're relative to this one (using their offset)
      val (lh, lleft) = if (left != null) (left.height, left.range.toAbs(left.key.off).left) else (0,0)
      val (rh, rright) = if (right != null) (right.height, right.range.toAbs(right.key.off).right) else (0,0)
      height = 1 + math.max(lh, rh)
      val newRange = OffsetRange(lleft, rright) // TODO: assign to range ?

      if (old != (height, newRange))
      {
        range = newRange
        if (null != parent) parent.adjustHeight
      }
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
        val sum = " (" + range + ") "

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
    var root: Tree = null
    var count = 0

    def get = root

    def asNodePos = if (root != null) root.withPos(0) else EmptyNodePos


    var debug = false

    var check = List[CharOffset]()

    override def toString: String =
    {
      if (null == root) "[]" else root.toString
    }

    def text =
    {

      val a = new Array[Char](count)

      def core(np: NodePos): Unit =
      {
        assert(0 <= np.absPos)
        assert(np.absPos < a.length, "np.absPos < a.length: (" + np.absPos + "," + a.length + ")")
        a(np.absPos) = np.n.key.char

        if (np.hasLeft) core(np.left)
        if (np.hasRight) core(np.right)
      }

      core( root.withPos(0) )
      a
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

    def find(i: Int): NodePos =
    {
      var n = NodePos(root, 0)
      var done = false


      //      while (!n.isNull && !done)
      //      {
      //        if (n.absPos == i) done = true
      //        else if (i < n.absPos) if (!n.hasLeft) done = true else n = n.left
      //        else if (n.absPos < i) if (!n.hasRight) done = true else n = n.right
      //        else assert(false, "find: impossible")
      //      }

      if (!n.isNull)
      {
        while (!done)
        {
          if (n.absPos == i) done = true
          else if (i < n.absPos) if (n.hasLeft) n = n.left else done = true
          else if (n.absPos < i) if (n.hasRight) n = n.right else done = true
          else assert(false, "find: impossible")
        }
      }

      n
    }


    def next(node: NodePos): NodePos =
    {
      @tailrec
      def leftDescendent(node: NodePos): NodePos =
      {
        if (node.hasNotLeft) node
        else leftDescendent(node.left)
      }

      @tailrec
      def rightAncestor(node: NodePos): NodePos =
      {
        if (node.hasNotParent) EmptyNodePos
        else
        {
          val p = node.parent
          if (p.absPos > node.absPos) p else rightAncestor(p)
        }
      }

      if (node.isNull) EmptyNodePos
      else
      {
        if (node.hasRight) leftDescendent(node.right)
        else rightAncestor(node)
      }

    }


    def rebalance(node: NodePos): NodePos =
    {
      def updateParent(in: NodePos, out: NodePos, parent: NodePos): Unit =
      {
        if (parent.isNotNull)
        {
          if (parent.left.n == out.n) parent.setLeft(in)
          else if (parent.right.n == out.n) parent.setRight(in)
          else assert(false, "reparent: parent -> in")
        }
        else
        {
          root = in.n
          root.key.off = in.absPos
        }
      }


      def rotateRight(out: NodePos): NodePos = // A <- *B -> C   ==>   A -> *B -> C
      {
        assert(out.isNotNull)
        assert(out.hasLeft)
        val parent = out.parent
        val in = out.left


        val inRight = in.right
        if (inRight.isNotNull) inRight.setParent(out)
        out.setLeft(inRight)

        out.setParent(in)
        in.setRight(out)

        in.setParent(parent)
        updateParent(in, out, parent)
        out.adjustHeight
        in
      }

      def rotateLeft(out: NodePos): NodePos = // A <- *B -> C   ==>   A <- *B <- C
      {
        // out - node that is being "rotated out"
        // in - node that is being "rotated in"

        assert(out.isNotNull)
        assert(out.hasRight)
        val parent = out.parent
        val in = out.right


        val inLeft = in.left
        if (inLeft.isNotNull) inLeft.setParent(out)
        out.setRight(inLeft)

        out.setParent(in)
        in.setLeft(out)

        in.setParent(parent)
        updateParent(in, out, parent)
        out.adjustHeight
        in
      }

      def h(node: NodePos) = if (node.isNotNull) node.n.height else 0

      def rebalanceRight(node: NodePos): NodePos =
      {
        assert(node.isNotNull)
        assert(node.hasLeft)

        if (h(node.left.right) > h(node.left.left)) rotateLeft(node)
        rotateRight(node)
      }

      def rebalanceLeft(node: NodePos): NodePos =
      {
        assert(node.isNotNull)
        assert(node.hasRight)

        if (h(node.right.left) > h(node.right.right)) rotateRight(node.right)
        rotateLeft(node)
      }


      if (node.isNotNull)
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


    def add(ch: Char, pos: Int): NodePos =
    {
      assert(pos >= 0)
      def dbgAdd: Unit =
      {
        val key = CharOffset(ch, pos)
        if (!check.exists(k => k == key)) check = check ::: List(key)
      }

      if (debug) dbgAdd

      val ret =
        if (null == root)
        {
          assert(pos == 0)
          count += 1
          root = new Tree(null, null, null, CharOffset(ch, pos))
          root.withPos(0)
        }
        else
        {
          val i = pos
          val parent = find(i)
          if (i < parent.absPos)
          {
            assert(!parent.hasLeft, "add: parent.left")
            count += 1
            parent.n.left = new Tree(null, null, parent.n, CharOffset(ch, pos))
            parent.left
          }
          else if (parent.absPos < i)
          {
            assert(!parent.hasRight, "add: parent.right")
            count += 1
            parent.n.right = new Tree(null, null, parent.n, CharOffset(ch, pos - parent.absPos))
            parent.right
          }
          else
          {
            assert(parent.absPos == i, "add: parent.key")
            parent.n.key.char = ch
            parent
          }
        }

      rebalance(ret)
      ret
    }

    def del(i: Int): Boolean =
    {
      val node = find(i)
      if (node.isNotNull && node.absPos == i)
      {
        count -= 1
        del(node)
        true
      }
      else false
    }

    def del(node: NodePos): Unit =
    {

      def dbgDel(i: Int): Unit =
      {
        //        println("// s.del(" + format(i) + ")")
        check = check.filter(key => key.off != i)
      }

      def promote(toPromote: NodePos, toSkip: NodePos): NodePos =
      {
        assert(toSkip.isNotNull, "promote: toSkip")
        assert(if (toPromote.isNotNull) toPromote.parent.n == toSkip.n else true, "promote: toPromote")

        val newParent = toSkip.parent

        if (newParent.isNotNull)
        {
          if (toSkip.n == newParent.left.n) newParent.setLeft(toPromote)
          else if (toSkip.n == newParent.right.n) newParent.setRight(toPromote)
          else assert(false, "promote: newParent->toSkip")
        }
        else
        {
          root = toPromote.n
          root.key.off = toPromote.absPos
        }

        if (toPromote.isNotNull) toPromote.setParent(newParent)
        newParent
      }


      def replaceWith(old: NodePos, newNode: NodePos): Unit =
      {
        old.n.key = newNode.n.key
        newNode.n.reset
      }

      if (node != null)
      {
        if (debug) dbgDel(node.absPos)

        val nextNode = next(node)
        if (nextNode.isNull)
        {
          assert(node.hasNotRight)
          val parent = promote(node.left, node)
          rebalance(parent)
        }
        else
        {
          if (nextNode.hasNotLeft)
          {
            val parent = promote(nextNode.right, nextNode)
            replaceWith(node, nextNode)
            rebalance(parent)

          }
          else
          {
            assert(node.hasNotRight)
            val parent = promote(node.left, node)
            rebalance(parent)
          }
        }


      }
    }

    def mergeWithRoot(l: NodePos, r: NodePos, parent: NodePos): NodePos =
    {
      def h(n: NodePos) = if (n.isNotNull) n.n.height else 0

      assert(parent.isNotNull)
      assert(parent.n.key.off >= 0)

      assert(if (l != null) l.absPos < parent.absPos else true)
      assert(if (r != null) parent.absPos < r.absPos else true)

      parent.setLeft(l)
      parent.setRight(r)
      if (l.isNotNull)
      {
        assert(l.n.key.off >= 0)
        l.setParent(parent)
      }

      if (r != null)
      {
        assert(r.n.key.off >= 0)
        r.setParent(parent)
      }

      parent.n.parent = null
      parent.adjustHeight

      parent
    }

    def mergeWithRootAVL(l: NodePos, r: NodePos, parent: NodePos): NodePos =
    {
      def h(n: NodePos) = if (n.isNotNull) n.n.height else 0

      assert(parent.isNotNull)
      assert(parent.n.key.off >= 0)

      assert(if (l != null) l.absPos < parent.absPos else true)
      assert(if (r != null) parent.absPos < r.absPos else true)

      if (math.abs(h(l) - h(r)) <= 1)
      {
        mergeWithRoot(l, r, parent)
      }
      else if (h(l) > h(r))
      {
        assert(l != null)
        val newRight = mergeWithRootAVL(l.right, r, parent)
        l.setRight(newRight)
        if (newRight.isNotNull) newRight.setParent(l)
        l.n.parent = null
        val out = rebalance(l)
        out
      }
      else
      {
        assert(h(l) < h(r))
        assert(r != null)

        val newLeft = mergeWithRootAVL(l, r.left, parent)
        r.setLeft(newLeft)
        if (newLeft.isNotNull) newLeft.setParent(r)
        r.n.parent = null
        val out = rebalance(r)
        out
      }


    }


    def split(x: Int): (Set, Set) =
    {

      // TODO
      def splitCore(node: NodePos, x: Int): (NodePos, NodePos) = // l.key <= x , x < r.key
      {
        if (node == null) (null, null)
        else
        {
          if (node.absPos <= x)
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

      val (left, right) = splitCore(root.withPos(0), x)
      if (left.isNotNull) left.setParent(EmptyNodePos)
      if (right.isNotNull) right.setParent(EmptyNodePos)

      root = null
      val setLeft = new Set;
      setLeft.root = left.n
      val setRight = new Set;
      setRight.root = right.n
      (setLeft, setRight)
    }

    def max: NodePos =
    {
      @tailrec
      def findMax(node: NodePos): NodePos =
      {
        assert(node != null)
        if (node.hasRight) findMax(node.right)
        else node
      }

      if (null != root) findMax(root.withPos(0)) else EmptyNodePos
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
        val rootPos = mergeWithRootAVL(left.root.withPos(0), right.root.withPos(0), parent)
        root = rootPos.n
        root.key.off = rootPos.absPos
      }
      else
      {
        root = right.root
      }

      left.root = null
      right.root = null
      this
    }


    // TODO*
    /*
     def sum(l: Int, r: Int): Long = sumSplitMerge(l, r) // sumNextLoop(l,r) //

     def sumSplitMerge(l: Int, r: Int): Long =
     {
       //    def addUp(node: Tree): Long =
       //    {
       //      if (null == node) 0
       //      else addUp(node.left) + addUp(node.right) + node.key
       //    }

       assert(l < M, "sum: hash")
       assert(r < M, "sum: hash")
       assert(l <= r, "sum: " + l + " > " + r)

       val s = split(l - 1)
       val (range, ignore) = s._2.split(r)

       val total = if (range.root != null) range.root.treeString else 0
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
    */

    //def findNext(i: Int): Tree = next(find(i))

    def isBinarySearch: Boolean =
    {
      def core(node: NodePos, min: NodePos, max: NodePos): Boolean =
      {
        def checkMin(min: NodePos, node: NodePos): Boolean =
        {
          if (null == node || null == min) true
          else min.absPos < node.absPos
        }

        def checkMax(node: NodePos, max: NodePos): Boolean =
        {
          if (null == max || null == node) true
          else node.absPos < max.absPos
        }

        if (node == null) true
        else checkMin(min, node) && checkMax(node, max) && core(node.left, min, node) && core(node.right, node, max)
      }

      core(root.withPos(0), NodePos(null, Int.MaxValue), NodePos(null, Int.MinValue))
    }
  }

  class TextCutter(in: String)
  {
    private val s = new Set
    in.foldLeft(0)((i,ch) => { s.add(ch, i); i+1 })

    override def toString: String = new String(chars)
    def chars = s.text

    // TODO
    def cutInsert(left: Int, right: Int, insert: Int) =
    {
      throw NotImplementedException
    }

  }

  def main(args: Array[String]) =
  {
    case class Op(left: Int, right: Int, insert: Int)

    val s: Scanner = if (args.isEmpty) new Scanner(System.in) else new Scanner(new File(args(0)))

    val text = s.next()
    val n = s.nextInt()

    val cutter = new TextCutter(text)
    for (i <- 0 to n - 1)
    {
      val left = s.nextInt
      val right = s.nextInt
      val insert = s.nextInt
      cutter.cutInsert(left, right, insert)
    }

    val out = cutter.toString
    println(out)
  }
}
