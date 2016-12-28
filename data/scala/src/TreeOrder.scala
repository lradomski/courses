import java.io.File
import java.util.Scanner

case class Vertex(key: Long, left: Int, right: Int)

class Node[T <% Ordered[T]](var left: Node[T], var right: Node[T], var parent: Node[T], var key: T)
{

}

object Node
{
}

object TreeOrder
{
  type Tree = Node[Long]

  def buildFrom(vs: Array[Vertex]): Tree =
  {
    def build(i: Int, parent: Tree): Tree =
    {
      if (i == -1) null
      else
      {
        val v = vs(i)
        val node = new Tree(null, null, null, v.key)
        node.left = build(v.left, node)
        node.right = build(v.right, node)
        node
      }
    }

    build(0, null)
  }

  def inorder(node: Tree, visit: Long => Unit): Unit =
  {
    if (node != null)
    {
      inorder(node.left, visit)
      visit(node.key)
      inorder(node.right, visit)
    }
  }

  def preorder(node: Tree, visit: Long => Unit): Unit =
  {
    if (node != null)
    {
      visit(node.key)
      preorder(node.left, visit)
      preorder(node.right, visit)
    }
  }

  def postorder(node: Tree, visit: Long => Unit): Unit =
  {
    if (node != null)
    {
      postorder(node.left, visit)
      postorder(node.right, visit)
      visit(node.key)
    }
  }



  def main(args: Array[String]) =
  {
    val s: Scanner = if (args.isEmpty) new Scanner(System.in) else new Scanner(new File(args(0)))

    val n = s.nextInt()

    def readVertices: Array[Vertex] =
    {
      val v = new Array[Vertex](n)
      for (i <- 0 to n - 1) v(i) = Vertex(s.nextLong, s.nextInt, s.nextInt)
      v
    }

    val tree = buildFrom(readVertices)
    def printKey(key: Long) = print(key + " ")
    inorder(tree, printKey); println()
    preorder(tree, printKey); println()
    postorder(tree, printKey); println()
  }
}
