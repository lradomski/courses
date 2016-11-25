import java.io.File
import java.util.Scanner


object NetPackSim {
  def main(args: Array[String]) =
  {
    //      for (i <- 1 to 24)
    //        {
    //          val name = "/Users/luke/git/courses/data/download/Starters PA1/tree_height/tests/%02d".format(i)
    //          val s : Scanner = new Scanner( new File(name) )
    //          val n = s.nextInt();
    //          val vertices = readVertices(s, n)
    //          val out = treeHeight(vertices)
    //
    //          val a = new Scanner( new File(name+".a")).nextInt();
    //          val res = (a==out);
    //          val line = "%02d".format(i)
    //          println(s"${line}: ${res} (${out},${a})");
    //        }

    val s : Scanner = if (args.isEmpty) new Scanner(System.in) else new Scanner( new File(args(0)) )

    val n = s.nextInt();
    val vertices = readVertices(s, n)

    val out = ""//treeHeight(vertices)

    if (args.isEmpty) println(out) else println(new Scanner( new File(args(0)+".a")).nextInt()==out );

    /*
    */
  }

  def readVertices(s: Scanner, n: Int): Array[Int] = {
    val tree = new Array[Int](n)
    for (i <- 0 to n - 1) {
      tree(i) = s.nextInt()
    }
    tree
  }

}
