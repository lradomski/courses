import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class SetSuite extends FunSuite
{
  def a(b:Boolean) = assert(b)

  test("add")
  {
    val s = new Set
    val keys = List(40, 10, 20, 15, 17, 30, 25, 26, 27)

    keys.foreach(key => s + key)
    keys.foreach(key => assert(s.exists(key)))

    // next
    val sorted = keys.sorted.reverse
    val t = s.get.parent
    sorted.foldLeft[s.Tree](null)((next, key) =>
    {
      val node = s.find(key)
      assert(node != null)
      val testNext = s.next(node)
      assert(next == testNext)
      node
    })

//    var n = s.get
//    a(n.key == 40)
//    a(n.right == null)
//    a(n.left.key == 10)
//    a(n.left.key =)

  }

  test("delete")
  {
    val s = new Set
    s + 1 - 1
    assert(s.get == null)

    s + 1 + 2 - 1
    assert(s.get.isLeaf)
    assert(s.get.key == 2)
    assert(s.exists(2))
    assert(!s.exists(1))
  }
}
