import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Prop.forAll
import org.scalatest.FunSuite
import org.scalatest.prop.Checkers
import Gen._
import Arbitrary.arbitrary

case class Node(var left: Node, var right: Node, var parent: Node, key: Int)
{
  if (left != null) left.parent = this
  if (right != null) right.parent = this

  override def toString = toString(this, 0)
  def toString(t: Node, indent: Int): String =
  {
    if (null == t) "[]"
    else
    {
      val s = " " * indent
      //"[*]"

      val ret = "\n" + s + "[ " + t.key + "\n" +
        s + "  <<<" + toString(t.left, indent + 5) + "\n" +
        s + "  >>>" + toString(t.right, indent + 5) + "\n" +
        s + "]\n"

      ret
    }
  }
}

val genNode = for {
  v <- Gen.choose(-100,100)
  left <- genTree
  right <- genTree
} yield Node(left, right, null, v)

def genLeaf = for {
  v <- arbitrary[Int]
} yield Node(null, null, null, v)

//def genTree: Gen[Node] = Gen.frequency(
//  (8, genLeaf),
//  (4, genNode)
//)

def genTree: Gen[Node] = oneOf(genNode, genLeaf)

genTree.sample