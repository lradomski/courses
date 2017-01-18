package drill


object TreeColumns
{

  case class TreeNode(left: TreeNode, right: TreeNode, value: Int)

  case class ListNode[T](var prev: ListNode[T], var next: ListNode[T], var value: T)

  type Nodes = List[TreeNode]
  type Columns = ListNode[List[TreeNode]]

  def columns(in: TreeNode): Columns =
  {
    def core(n: TreeNode, col: Columns): Unit =
    {
      if (n.left != null)
      {
        if (col.prev == null)
        {
          col.prev = new ListNode[Nodes](null, col, List())
        }
        core(n.left, col.prev)
      }

      if (n.right != null)
      {
        if (col.next == null)
        {
          col.next = new ListNode[Nodes](col, null, List())
        }
        core(n.right, col.next)
      }

      col.value = n :: col.value
    }

    val cols = ListNode[Nodes](null, null, List())
    core(in, cols)
    cols
  }

  def printCols(in: Columns): Unit =
  {
    var col = in
    while (col.prev != null) col = col.prev

    val first = col

    var allEmpty = true

    do
    {
      allEmpty = true
      var c = first
      while (c != null)
      {
        if (c.value.isEmpty)
        {
          print("\t")
        }
        else
        {
          allEmpty = false
          print(c.value.head.value + "\t")
          c.value = c.value.tail
        }

        c = c.next
      }

      println
    } while (!allEmpty)

  }

  def main(args: Array[String]): Unit =
  {
    val tree = TreeNode(
      TreeNode(
        TreeNode(null, null, 20),
        TreeNode(null, null, 30),
        2),
      TreeNode(
        TreeNode(null, null, 200),
        TreeNode(null, null, 300),
        3),
      1
    )

    val cols = columns(tree)

    printCols(cols)
  }

}
