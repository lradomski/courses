import java.io.File
import java.util.Scanner

import scala.annotation.tailrec

case class Vertex(key: Long, left: Int, right: Int)


object TreeOrder
{
  class Node[T <% Ordered[T]](var left: Node[T], var right: Node[T], var parent: Node[T], var key: T)
  {

  }

  type Tree = Node[Long]

  def buildFrom(vs: Array[Vertex]): Tree =
  {
    val ns = vs.map(v => new Tree(null, null, null, v.key))

    def setParent(i: Int, parent: Tree) =
    {
      if (i == -1) null
      else
      {
        val n = ns(i)
        n.parent = parent
        n
      }
    }

    for (i <- 0 to vs.length - 1)
    {
      val node = ns(i)
      val v = vs(i)
      node.left = setParent(v.left, node)
      node.right = setParent(v.right, node)
    }

    if (ns.length > 0) ns(0) else null
  }

  def buildFrom2(vs: Array[Vertex]): Tree =
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


  trait Visitor
  {
    def visit: List[Visitor]
  }

  case class NodePrint(node: Tree) extends Visitor
  {
    override def visit: List[Visitor] =
    {
      print(node.key + " ")
      List()
    }
  }

  case class PreOrder(node: Tree) extends Visitor
  {
    override def visit: List[Visitor] =
    {
      if (node == null) List()
      else List(NodePrint(node), PreOrder(node.left), PreOrder(node.right))
    }
  }

  case class InOrder(node: Tree) extends Visitor
  {
    override def visit: List[Visitor] =
    {
      if (node == null) List()
      else List(InOrder(node.left), NodePrint(node), InOrder(node.right))
    }
  }

  case class PostOrder(node: Tree) extends Visitor
  {
    override def visit: List[Visitor] =
    {
      if (node == null) List()
      else List(PostOrder(node.left), PostOrder(node.right), NodePrint(node))
    }
  }


  @tailrec
  def visit(todo: List[Visitor]): Unit =
  {
    if (!todo.isEmpty)
    {
      visit(todo.head.visit ::: todo.tail)
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

//    def printKey(key: Long) = print(key + " ")
//    inorder(tree, printKey);
//    println()
//    preorder(tree, printKey);
//    println()
//    postorder(tree, printKey);
//    println()
//
//    println()

    visit(List(InOrder(tree)));
    println;
    visit(List(PreOrder(tree)));
    println;
    visit(List(PostOrder(tree)));
    println;

  }
}
