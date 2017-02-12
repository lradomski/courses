package drill

case class Node[T](var left: Node[T], var right: Node[T], var parent: Node[T], var key: T)
{
  setLeft(left)
  setRight(right)

  override def toString: String =
  {
    val l = if (left != null) left.toString + " " else ""
    val r = if (right != null) " " + right.toString else ""
    l + key.toString + r
  }

  def setLeft(node: Node[T]): Node[T] =
  {
    left = node
    if (node != null) node.parent = this
    this
  }

  def setRight(node: Node[T]): Node[T] =
  {
    right = node
    if (node != null) node.parent = this
    this
  }

  def unparent(): Node[T] =
  {
    if (parent != null)
    {
      if (parent.left == this) parent.left = null
      else if (parent.right == this) parent.right = null
      else assert(false, "Node.unparent - ?")
    }

    this
  }

  def makeSetLeft(node: Node[T]) = () => setLeft(node)

  def makeSetRight(node: Node[T]) = () => setRight(node)

  def makeUnparent() = () => unparent()

  val makeNone = () => this

  def isLeftChild = parent != null && parent.left == this
  def isRightChild = parent != null && parent.right == this
  def isRoot = parent == null

  def hasLeft = left != null
  def hasNotLeft = left == null
  def hasRight = right != null
  def hasNotRight = right == null

  def rotateRight() =
  {
    assume(left != null, "Node.rotateRight - left ?")

    val p = parent
    val in = left
    val out = this
    val switch = left.right

    var actions = Seq(
      if (isLeftChild) p.makeSetLeft(in)
      else if (isRightChild) p.makeSetRight(in)
      else in.makeUnparent(),

      in.makeSetRight(out),

      out.makeSetLeft(switch)
    )

    actions.foreach(_ ())
    in
  }

  def rotateLeft() =
  {
    assume(right != null, "Node.rotateLeft - right ?")
    val p = parent
    val out = this
    val in = right
    val switch = right.left

    val actions = Seq(
      if (isLeftChild) p.makeSetLeft(in)
      else if (isRightChild) p.makeSetRight(in)
      else in.makeUnparent(),

      in.makeSetLeft(out),

      out.makeSetRight(switch)
    )

    actions.foreach(_ ())
    in
  }
}

object Node
{
  def rotateEnabled[T](n: Node[T], left: Boolean) =
  {
    def core(n: Node[T], ns: List[Node[T]]): List[Node[T]] =
    {
      if (n != null)
      {
        val nsNew = if (left && n.hasRight || !left && n.hasLeft) n :: ns else ns
        core(n.left, core(n.right, nsNew))
      }
      else ns
    }

    core(n, List())
  }
}

class TreeGen
{

}
