import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class SetSuite extends FunSuite
{
  def a(b: Boolean) = assert(b)

  def makeFrom(keys: List[Int]): Set = makeFrom(keys, false)

  def makeFrom(keys: List[Int], debug: Boolean): Set =
  {
    val s = new Set
    s.debug = debug
    val set = keys.foldLeft(s)((set, key) => set + key)
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
    has.foreach(key => assert(s.exists(key), "key=" + key))
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

  def verifyB(s: Set, has: List[Int], hasNot: List[Int]): Set =
  {
    assert(s.isBalanced)
    verify(s, has, hasNot, has.sorted)
  }

  test("delete/simple")
  {
    verifyB(makeFrom(List(2, 3, 1)) - 2, List(3, 1), List(2))
    verifyB(makeFrom(List(2, 3, 1)) - 1, List(2, 3), List(1))
    verifyB(makeFrom(List(2, 3, 1)) - 3, List(2, 1), List(3))

    assert(((new Set) - 10).get == null)
  }

  test("delete/next/left-descendent/1")
  {
    val keys = List(20, 10, 30, 25, 40, 35, 50, 37, 36, 38)
    val deleted = 30
    val has = keys.filter(key => key != deleted)
    val hasNot = List(deleted)
    verifyB(makeFrom(keys) - deleted, has, hasNot)
  }

  test("delete/next/left-descendent/2")
  {
    val keys = List(20, 10, 30, 40, 35, 50, 37, 36, 38)
    val deleted = 30
    val has = keys.filter(key => key != deleted)
    val hasNot = List(deleted)
    verifyB(makeFrom(keys) - deleted, has, hasNot)
  }

  test("delete/next/right-ancestor/1")
  {
    val keys = List(40, 10, 20, 15, 17, 30, 25, 26, 27)
    val deleted = 30
    val has = keys.filter(key => key != deleted)
    val hasNot = List(deleted)
    verifyB(makeFrom(keys) - deleted, has, hasNot)
  }

  test("delete/next/right-ancestor/2")
  {
    val keys = List(10, 20, 30, 25, 23)
    val deleted = 30
    val has = keys.filter(key => key != deleted)
    val hasNot = List(deleted)
    verifyB(makeFrom(keys) - deleted, has, hasNot)
  }


  test("case20")
  {
    val s = new Set
    //s.debug = true
    s.add(66257759)
    s.sum(25751289, 70170547) // = 66257759 // 66257759 // List(66257759)
    s.sum(94506006, 684106853) // = 0 // 0 // List()
    s.add(477444954)
    s.sum(400483980, 423330836) // = 0 // 0 // List()
    s.del(477444954)
    s.sum(66257759, 66257759) // = 66257759 // 66257759 // List(66257759)
    s.sum(614922908, 991893293) // = 0 // 0 // List()
    s.sum(66257759, 66257759) // = 66257759 // 66257759 // List(66257759)
    s.add(729563666)
    s.sum(381066809, 751489076) // = 729563666 // 729563666 // List(729563666)
    s.del(729563666)
    s.sum(217022539, 332199166) // = 0 // 0 // List()
    s.sum(66257759, 66257759) // = 66257759 // 66257759 // List(66257759)
    s.sum(254442848, 840344692) // = 0 // 0 // List()
    s.sum(66257759, 66257759) // = 66257759 // 66257759 // List(66257759)
    s.sum(66257759, 66257759) // = 66257759 // 66257759 // List(66257759)
    s.sum(66257759, 66257759) // = 66257759 // 66257759 // List(66257759)
    s.sum(755518151, 894127603) // = 0 // 0 // List()
    s.del(66257759)
    s.sum(412617563, 631410280) // = 0 // 0 // List()
    s.add(601030115)
    s.sum(257003372, 887483600) // = 601030115 // 601030115 // List(601030115)
    s.add(755077338)
    s.add(579842587)
    s.sum(579842587, 755077338) // = 1935950040 // 1935950040 // List(601030115, 755077338, 579842587)
    s.sum(579842587, 755077338) // = 1935950040 // 1935950040 // List(601030115, 755077338, 579842587)
    s.add(574848345)
    s.add(442659306)
    s.del(442659306)
    s.add(680116571)
    s.del(574848345)
    s.add(31190791)
    s.sum(933298871, 999728041) // = 0 // 0 // List()
    s.sum(21011834, 570648768) // = 31190791 // 31190791 // List(31190791)
    s.add(248399406)
    s.add(433103322)
    s.sum(31190791, 755077338) // = 3328760130 // 3328760130 // List(601030115, 755077338, 579842587, 680116571, 31190791, 248399406, 433103322)
    s.add(871353531)
    val sum1 = s.sum(31190791, 871353531) // = 2764919752 // 4200113661 // List(601030115, 755077338, 579842587, 680116571, 31190791, 248399406, 433103322, 871353531)
    assert(sum1 == 4200113661L)
  }

  test("split/merge/case20")
  {

    def permute(keys: List[Int]): Unit =
    {
      def sum(l: List[Int]) = l.foldLeft(0L)((sum, key) => sum + key)
      val s = makeFrom(keys) //, true)

      //s.sum(2, 2) // = 2 // 2 // List(2)
//      s.sum(2, 3) // = 5 // 5 // List(2, 3)
//      verify(s, keys, List())
      //    assert(s.sum(248399406, 579842587) == 1261345315)

      //    keys.foreach(key => assert(s.sum(key,key) == key))
      //s.sum(31190791, 755077338)
      //s.sum(248399406, 433103322)
      verify(s, keys, List())


      keys.sorted.foreach(left =>
      {
        keys.sorted.foreach(right =>
        {
          if (left <= right)
          {
            assert(s.sum(left, right) == sum(keys.filter(key => left <= key && key <= right)), "[" + left + "," + right + "]")
            verify(s, keys, List())
          }
        })
      })
    }

    permute(List(1, 2, 3))

    val keys = List(601030115, 755077338, 579842587, 680116571, 31190791, 248399406, 433103322, 871353531)
    permute(keys)

  }

  test("split/merge/simple")
  {
    val s = makeFrom(List(1, 2, 3))

    val (sL, sR) = s.split(2)
    verify(sL, List(1, 2), List(3))
    verify(sR, List(3), List(1, 2))
    verify(s, List(), List(1, 2, 3))


    s.merge(sL, sR)
    verifyB(s, List(1, 2, 3), List())
    verify(sL, List(), List(1, 2, 3))
    verify(sR, List(), List(1, 2, 3))

    verify(makeFrom(List()).merge(makeFrom(List()), makeFrom(List(1, 2, 3))), List(1, 2, 3), List())

  }

  test("split/merge/2")
  {
    val keys = List(26, 20, 40, 15, 25, 30, 50, 10, 17, 29, 35)
    val s = makeFrom(keys)
    val (sl, sr) = s.split(30)
    verify(sl, keys.filter(key => key <= 30), keys.filter(key => key > 30))
    verify(sr, keys.filter(key => key > 30), keys.filter(key => key <= 30))
    verify(s, List(), keys)

    s.merge(sl, sr)
    verifyB(s, keys, List())

    val (sl2, sr2) = s.split(23)
    verify(sl2, keys.filter(key => key <= 23), keys.filter(key => key > 23))
    verify(sr2, keys.filter(key => key > 23), keys.filter(key => key <= 23))
    verify(s, List(), keys)
    s.merge(sl2, sr2)

    val min = keys.foldLeft(keys.head)((min, key) => if (key < min) key else min)
    val (sl3, sr3) = s.split(min - 1)
    verify(sl3, List(), keys)
    verify(sr3, keys, List())
    verify(s, List(), keys)
    s.merge(sl3, sr3)

    val max = keys.foldLeft(keys.head)((max, key) => if (key > max) key else max)
    val (sl4, sr4) = s.split(max + 1)
    verify(sl4, keys, List())
    verify(sr4, List(), keys)
    verify(s, List(), keys)
    s.merge(sl4, sr4)
  }

  test("sum/1")
  {
    val keys = List(26, 20, 40, 15, 25, 30, 50, 10, 17, 29, 35)
    val s = makeFrom(keys)
    assert(s.isBalanced)

    assert(s.sum(40, 50) == 90)
    assert(s.isBalanced)

    assert(s.sum(0, 0) == 0)
    assert(s.isBalanced)

    assert(s.sum(0, 10) == 10)
    assert(s.isBalanced)

    assert(s.sum(23, 23) == 0)
    assert(s.isBalanced)

    assert(s.sum(23, 45) == keys.foldLeft(0)((sum, key) => if (23 <= key && key <= 45) sum + key else sum))
    assert(s.isBalanced)
  }
}
