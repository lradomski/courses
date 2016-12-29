import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class SetSuite extends FunSuite
{
  def a(b: Boolean) = assert(b)

  def makeFrom(keys: List[Int]) =
  {
    val set = keys.foldLeft(new Set)((set, key) => set + key)
    assert(set.isBinarySearch)
    assert(set.isBalanced)
    set
  }


  test("add")
  {
    val keys = List(40, 10, 20, 15, 17, 30, 25, 26, 27)

    val s = makeFrom(keys)
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

  }

  test("next")
  {
    var keys = List(1)
    assert(makeFrom(keys).findNext(keys.head) == null)

    // * -> right
    keys = List(10, 20)
    assert(makeFrom(keys).findNext(10).key == 20)

    // * -> right -> left
    keys = List(10, 20, 15)
    assert(makeFrom(keys).findNext(10).key == 15)

    // * -> right -> left -> left
    keys = List(10, 20, 25, 15, 13)
    assert(makeFrom(keys).findNext(10).key == 13)

    // * -> right -> left -> (->left,right) -> left
    keys = List(10, 20, 25, 15, 13, 11)
    assert(makeFrom(keys).findNext(10).key == 11)
  }

  test("next/right ancestor")
  {
    var keys = List(10, 20, 25, 15, 13, 11)
    assert(makeFrom(keys).findNext(25) == null)

    keys = List(40, 30, 10, 20, 25, 15, 13, 11)
    assert(makeFrom(keys).findNext(25).key == 30)

    keys = List(40, 30, 20, 25, 15, 13, 11)
    assert(makeFrom(keys).findNext(25).key == 30)
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

  def verify(s: Set, has: List[Int], hasNot: List[Int], sequence: List[Int]): Set =
  {
    assert(s.isBinarySearch)
    has.foreach(key => assert(s.exists(key)))
    hasNot.foreach(key => assert(!s.exists(key)))

    sequence.foldRight[s.Tree](null)((key, next) =>
    {
      val node = s.find(key)
      assert(node != null)
      val testNext = s.next(node)
      assert(next == testNext)
      node
    })
    s
  }

  def verify(s: Set, has: List[Int], hasNot: List[Int]): Set = verify(s, has, hasNot, has.sorted)

  test("delete/simple")
  {
    verify(makeFrom(List(2, 3, 1)) - 2, List(3, 1), List(2))
    verify(makeFrom(List(2, 3, 1)) - 1, List(2, 3), List(1))
    verify(makeFrom(List(2, 3, 1)) - 3, List(2, 1), List(3))

    assert(((new Set) - 10).get == null)
  }

  test("delete/next/left-descendent/1")
  {
    val keys = List(20, 10, 30, 25, 40, 35, 50, 37, 36, 38)
    val deleted = 30
    val has = keys.filter(key => key != deleted)
    val hasNot = List(deleted)
    verify(makeFrom(keys) - deleted, has, hasNot)
  }

  test("delete/next/left-descendent/2")
  {
    val keys = List(20, 10, 30, 40, 35, 50, 37, 36, 38)
    val deleted = 30
    val has = keys.filter(key => key != deleted)
    val hasNot = List(deleted)
    verify(makeFrom(keys) - deleted, has, hasNot)
  }

  test("delete/next/right-ancestor/1")
  {
    val keys = List(40, 10, 20, 15, 17, 30, 25, 26, 27)
    val deleted = 30
    val has = keys.filter(key => key != deleted)
    val hasNot = List(deleted)
    verify(makeFrom(keys) - deleted, has, hasNot)
  }

  test("delete/next/right-ancestor/2")
  {
    val keys = List(10, 20, 30, 25, 23)
    val deleted = 30
    val has = keys.filter(key => key != deleted)
    val hasNot = List(deleted)
    verify(makeFrom(keys) - deleted, has, hasNot)
  }

  test("case20")
  {
    val s = new Set
    s.s(40279559, 89162572) // = 0 // 0 // List()
    s - 774613289
    s.s(869592654, 915517087) // = 0 // 0 // List()
    s - 165280355
    s - 776346290
    s - 221187096
    s.s(421986248, 742826969) // = 0 // 0 // List()
    s.s(83228103, 852190011) // = 0 // 0 // List()
    s - 640319482
    s - 617070033
    s + 66257759
    s.s(25751289, 70170547) // = 66257759 // 66257759 // List(66257759)
    s.s(94506006, 684106853) // = 0 // 0 // List()
    s - 954357244
    s + 477444954
    s.s(400483980, 423330836) // = 0 // 0 // List()
    s - 477444954
    s.s(66257759, 66257759) // = 66257759 // 66257759 // List(66257759)
    s - 888475917
    s.s(614922908, 991893293) // = 0 // 0 // List()
    s.s(66257759, 66257759) // = 66257759 // 66257759 // List(66257759)
    s + 729563666
    s.s(381066809, 751489076) // = 729563666 // 729563666 // List(729563666)
    s - 729563666
    s.s(217022539, 332199166) // = 0 // 0 // List()
    s.s(66257759, 66257759) // = 66257759 // 66257759 // List(66257759)
    s.s(254442848, 840344692) // = 0 // 0 // List()
    s - 322445571
    s.s(66257759, 66257759) // = 66257759 // 66257759 // List(66257759)
    s - 880381743
    s.s(66257759, 66257759) // = 66257759 // 66257759 // List(66257759)
    s.s(66257759, 66257759) // = 66257759 // 66257759 // List(66257759)
    s.s(755518151, 894127603) // = 0 // 0 // List()
    s - 66257759
    s.s(412617563, 631410280) // = 0 // 0 // List()
    s - 463415495
    s + 601030115
    s.s(257003372, 887483600) // = 601030115 // 601030115 // List(601030115)
    s + 755077338
    s + 579842587
    s.s(579842587, 755077338) // = 1935950040 // 1935950040 // List(601030115, 755077338, 579842587)
    s - 951668344
    s.s(579842587, 755077338) // = 1935950040 // 1935950040 // List(601030115, 755077338, 579842587)
    s + 574848345
    s + 442659306
    s - 442659306
    s + 680116571
    s - 574848345
    s + 31190791
    s.s(933298871, 999728041) // = 0 // 0 // List()
    s.s(21011834, 570648768) // = 31190791 // 31190791 // List(31190791)
    s + 248399406
    s + 433103322
    s.s(31190791, 755077338) // = 3328760130 // 3328760130 // List(601030115, 755077338, 579842587, 680116571, 31190791, 248399406, 433103322)
    s + 871353531
    s.s(31190791, 871353531) // = 4200113661 // 4200113661 // List(601030115, 755077338, 579842587, 680116571, 31190791, 248399406, 433103322, 871353531)
    s.s(31190791, 871353531) // = 4200113661 // 4200113661 // List(601030115, 755077338, 579842587, 680116571, 31190791, 248399406, 433103322, 871353531)
    s + 118055263
    s.s(533468479, 690718988) // = 0 // 1860989273 // List(601030115, 579842587, 680116571)
    assert(s.isBalanced)
    assert(s.isBinarySearch)
  }


}
