package drill

import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Prop.forAll
import org.scalatest.FunSuite
import org.scalatest.prop.Checkers
import Gen._
import Arbitrary.arbitrary

class RopeTest
  extends FunSuite with Checkers
{
  test("tree-gen")
  {
    case class Node(var left: Node, var right: Node, var parent: Node, v: Int)
    {
      if (left != null) left.parent = this
      if (right != null) right.parent = this

    }

//    val genNode = for {
//      v <- arbitrary[Int]
//      left <- genTree
//      right <- genTree
//    } yield Node(left, right, null, v)
//
//    def genTree: Gen[Node] = oneOf(genNode, null)


  }

}
