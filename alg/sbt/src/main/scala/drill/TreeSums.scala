package drill


object TreeSums
{

  case class Node(left: Node, right: Node, value: Int)

  def paths(in: Node, total: Long): List[List[Int]] =
  {
    def core(n: Node, path: List[Int], acc: List[List[Int]]): List[List[Int]] =
    {
      def subPaths(path: List[Int]): List[List[Int]] =
      {
        if (path.isEmpty) List()
        else
        {
          for
            {
              i <- (1 to path.length).toList
              subPath = path.take(i)
              if subPath.sum <= total  // or ==
            } yield subPath
        }
      }

      if (n == null) acc
      else
      {
        val newPath = n.value :: path
        val newAcc = subPaths(newPath) ::: acc

        core(n.right, newPath, core(n.left, newPath, newAcc))
      }
    }

    core(in, List(), List())
  }

  def main(args: Array[String]): Unit =
  {
    val tree =
      Node(
        Node(null, null, 2),
        Node(null, null, 3),
        1
      )

    val ps = paths(tree, 100)
    for (p <- ps) println(p)
  }

}
