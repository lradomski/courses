package drill

import scala.collection.mutable.Queue


object Tree
{

  class Node(var left: Node, var right: Node, var parent: Node, var value: Int)
  {
    override def toString = value.toString
  }

  def leaves(in: Node): List[Int] =
  {
    case class Move(val ascend: Boolean, val left: Boolean) // ascend/descend, from left or right

    def move(n: Node, m: Move): (Node, Move) = // ascend/descend
    {

      // desc ? -> left ? -> right ?
      // asc,left -> desc,right ? -> asc,*
      // asc,right -> asc,*

      def moveUp: (Node, Move) =
      {
        if (n.parent.left == n) (n.parent, Move(true, true))
        else if (n.parent.right == n) (n.parent, Move(true, false))
        else (null, Move(true, true)) // mustn't happen
      }

      if (m.ascend)
      {
        if (m.left && n.right != null) (n.right, Move(false, false))
        else if (n.parent != null) moveUp
        else (null, Move(true, true))
      }
      else
      {
        if (n.left != null) (n.left, Move(false, true))
        else if (n.right != null) (n.right, Move(false, false))
        else if (n.parent != null) moveUp
        else (null, Move(true, true))
      }
    }

    def unparent(n: Node) =
    {
      val p = n.parent
      if (p != null)
      {
        if (p.left == n) p.left = null
        else if (p.right == n) p.right = null

        n.parent = null
      }
      p
    }

    var done = false

    var lastMove = Move(false, true)
    var n = in
    val out = new Queue[Int]()
    val nodes = new Queue[Node]()


    while (!done)
    {
      val d_out = out
      val d_nodes = nodes

      var doneMoving = false

      while (n != null)
      {
        val d_out = out
        val d_nodes = nodes
        val d_n = n

        if (n.left == null && n.right == null) nodes.enqueue(n)
        val nm = move(n, lastMove)
        n = nm._1
        lastMove = nm._2
      }

      if (nodes.isEmpty) done = true
      else
      {
        val nm = move(nodes.head, Move(false, true))
        n = nm._1
        lastMove = nm._2

        while (!nodes.isEmpty)
        {
          val nOut = nodes.dequeue()
          out.enqueue(nOut.value)
          unparent(nOut)
        }
      }

      assert(nodes.isEmpty)
    }

    out.toList
  }

  def leaves2(in: Node): List[Int] =
  {
    def unparent(n: Node): Node =
    {
      if (n.parent != null)
      {
        val parent = n.parent
        if (parent.left == n) parent.left == null
        else
        {
          assert(parent.right == n)
          n.right == null
        }
        parent
      }
      else null
    }

    if (in == null) List()
    else
    {
      var tree = new Queue[Node]()
      var parents = new Queue[Node]
      var out = new Queue[Int]

      assert(in == null)
      tree.enqueue(in)

      while (!tree.isEmpty)
      {
        val n = tree.head
        tree.dequeue

        if (n.left != null) tree.enqueue(n.left)
        if (n.right != null) tree.enqueue(n.right)

        if (n.left == null && n.right == null)
        {
          val p = unparent(n)
          if (p != null) parents.enqueue(p)
          out.enqueue(n.value)
        }
      }

      while (!parents.isEmpty)
      {
        val n = parents.head
        parents.dequeue

        if (n.left == null && n.right == null)
        {
          val p = unparent(n)
          if (p != null) parents.enqueue(p)
          out.enqueue(n.value)
        }
      }

      out.toList
    }


  }

  def main(args: Array[String]) =
  {
    case class Vertex(key: Int, left: Int, right: Int)

    def buildFrom(in: Array[Vertex]): Node =
    {
      val vs = in.sortBy(_.key)
      val ns = vs.map(v => new Node(null, null, null, v.key))

      def setParent(i: Int, parent: Node) =
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
        assert(ns(i).value == i, ns(i).value + " != " + i)
        val v = vs(i)
        node.left = setParent(v.left, node)
        node.right = setParent(v.right, node)
      }

      if (ns.length > 0) ns(ns.length - 1) else null
    }

//    val tree = buildFrom(
//      Array(
//        Vertex(2, 0, 1),
//        Vertex(0, -1, -1),
//        Vertex(1, -1, -1)
//      )
//    )

        val tree = buildFrom(
          Array(
            Vertex(8,6,7),
              Vertex(6,0,4),
                  Vertex(0,-1,-1),
                  Vertex(4,-1,1),
                    // -
                    Vertex(1,-1,-1),
              Vertex(7,2,5),
                  Vertex(2,-1,-1),
                  Vertex(5,3,-1),
                    Vertex(3,-1,-1)
                    // -
          ))

    val l = leaves(tree)
    l.foreach(i => print(i + " "))
    println
  }

}
