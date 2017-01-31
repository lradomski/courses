import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import RangeSum.{Set,Tree}

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
    sorted.foldLeft[Tree](null)((next, key) =>
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

    sequence.foldRight[Tree](null)((key, next) =>
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

  test("case36")
  {
    val s = new Set
    s.sum(209027527, 465681241) // = 0 // 0 // List()
    s.sum(689656112, 799574286) // = 0 // 0 // List()
    s.sum(487889785, 593663747) // = 0 // 0 // List()
    s.sum(28062399, 437825162) // = 0 // 0 // List()
    s.sum(205291457, 515367700) // = 0 // 0 // List()
    s.add(554939149)
    s.add(300593336)
    s.add(595570265)
    s.sum(146900570, 390853981) // = 300593336 // 300593336 // List(300593336)
    s.add(712580963)
    s.del(554939149)
    s.add(527913028)
    s.add(977975530)
    s.add(929644014)
    s.sum(142151811, 518103735) // = 300593336 // 300593336 // List(300593336)
    s.del(712580963)
    s.sum(300593336, 977975530) // = 3331696173 // 3331696173 // List(300593336, 595570265, 527913028, 977975530, 929644014)
    s.sum(280544957, 288527231) // = 0 // 0 // List()
    s.del(929644014)
    s.add(364027698)
    s.sum(674697217, 752227904) // = 0 // 0 // List()
    s.sum(300593336, 977975530) // = 2766079857 // 2766079857 // List(300593336, 595570265, 527913028, 977975530, 364027698)
    s.add(136624696)
    s.sum(136624696, 977975530) // = 2902704553 // 2902704553 // List(300593336, 595570265, 527913028, 977975530, 364027698, 136624696)
    s.sum(136624696, 977975530) // = 2902704553 // 2902704553 // List(300593336, 595570265, 527913028, 977975530, 364027698, 136624696)
    s.sum(537200992, 978195383) // = 1573545795 // 1573545795 // List(595570265, 977975530)
    s.add(773176425)
    s.add(293228836)
    s.add(885450301)
    s.add(727790067)
    s.del(293228836)
    s.del(727790067)
    s.add(21713170)
    s.sum(253536027, 517084190) // = 664621034 // 664621034 // List(300593336, 364027698)
    s.add(641411075)
    s.add(47253166)
    s.sum(21713170, 977975530) // = 5271708690 // 5271708690 // List(300593336, 595570265, 527913028, 977975530, 364027698, 136624696, 773176425, 885450301, 21713170, 641411075, 47253166)
    s.add(218051575)
    s.add(627262505)
    s.sum(21713170, 977975530) // = 6117022770 // 6117022770 // List(300593336, 595570265, 527913028, 977975530, 364027698, 136624696, 773176425, 885450301, 21713170, 641411075, 47253166, 218051575, 627262505)
    s.sum(101500738, 479327679) // = 1019297305 // 1019297305 // List(300593336, 364027698, 136624696, 218051575)
    s.sum(21713170, 977975530) // = 6117022770 // 6117022770 // List(300593336, 595570265, 527913028, 977975530, 364027698, 136624696, 773176425, 885450301, 21713170, 641411075, 47253166, 218051575, 627262505)
    s.sum(358456690, 805390139) // = 3529360996 // 3529360996 // List(595570265, 527913028, 364027698, 773176425, 641411075, 627262505)
    s.sum(21713170, 977975530) // = 6117022770 // 6117022770 // List(300593336, 595570265, 527913028, 977975530, 364027698, 136624696, 773176425, 885450301, 21713170, 641411075, 47253166, 218051575, 627262505)
    s.del(527913028)
    s.del(136624696)
    s.add(459880895)
    s.sum(21713170, 977975530) // = 5912365941 // 5912365941 // List(300593336, 595570265, 977975530, 364027698, 773176425, 885450301, 21713170, 641411075, 47253166, 218051575, 627262505, 459880895)
    s.del(773176425)
    s.del(977975530)
    s.sum(143710071, 715474105) // = 3206797349 // 3206797349 // List(300593336, 595570265, 364027698, 641411075, 218051575, 627262505, 459880895)
    s.del(595570265)
    s.add(453877608)
    s.sum(623320386, 980511326) // = 2154123881 // 2154123881 // List(885450301, 641411075, 627262505)
    s.del(218051575)
    s.del(627262505)
    s.add(43710223)
    s.sum(21713170, 885450301) // = 3217917472 // 3217917472 // List(300593336, 364027698, 885450301, 21713170, 641411075, 47253166, 459880895, 453877608, 43710223)
    s.add(831654837)
    s.del(831654837)
    s.add(355653614)
    s.sum(71400418, 466570877) // = 1934033151 // 1934033151 // List(300593336, 364027698, 459880895, 453877608, 355653614)
    s.sum(21713170, 885450301) // = 3573571086 // 3573571086 // List(300593336, 364027698, 885450301, 21713170, 641411075, 47253166, 459880895, 453877608, 43710223, 355653614)
    s.sum(21713170, 885450301) // = 3573571086 // 3573571086 // List(300593336, 364027698, 885450301, 21713170, 641411075, 47253166, 459880895, 453877608, 43710223, 355653614)
    s.del(300593336)
    s.del(355653614)
    s.del(364027698)
    s.add(555846477)
    s.del(885450301)
    s.sum(804308943, 911743377) // = 0 // 0 // List()
    s.sum(21713170, 641411075) // = 2223692614 // 2223692614 // List(21713170, 641411075, 47253166, 459880895, 453877608, 43710223, 555846477)
    s.sum(21713170, 641411075) // = 2223692614 // 2223692614 // List(21713170, 641411075, 47253166, 459880895, 453877608, 43710223, 555846477)
    s.add(815640567)
    s.del(453877608)
    s.add(757804188)
    s.add(704607002)
    s.sum(479299135, 487603328) // = 0 // 0 // List()
    s.add(120759900)
    s.add(344374296)
    s.del(641411075)
    s.sum(352958324, 926938085) // = 3293779129 // 3293779129 // List(459880895, 555846477, 815640567, 757804188, 704607002)
    s.sum(21713170, 815640567) // = 3871589884 // 3871589884 // List(21713170, 47253166, 459880895, 43710223, 555846477, 815640567, 757804188, 704607002, 120759900, 344374296)
    s.add(66792035)
    s.sum(21713170, 815640567) // = 3938381919 // 3938381919 // List(21713170, 47253166, 459880895, 43710223, 555846477, 815640567, 757804188, 704607002, 120759900, 344374296, 66792035)
    s.sum(394165432, 496316304) // = 459880895 // 459880895 // List(459880895)
    s.add(515489060)
    s.sum(21713170, 815640567) // = 4453870979 // 4453870979 // List(21713170, 47253166, 459880895, 43710223, 555846477, 815640567, 757804188, 704607002, 120759900, 344374296, 66792035, 515489060)
    s.add(568048905)
    s.sum(21713170, 815640567) // = 5021919884 // 5021919884 // List(21713170, 47253166, 459880895, 43710223, 555846477, 815640567, 757804188, 704607002, 120759900, 344374296, 66792035, 515489060, 568048905)
    s.add(418462633)
    s.del(344374296)
    s.add(535030022)
    s.del(459880895)
    s.del(120759900)
    s.sum(21713170, 815640567) // = 5050397448 // 5050397448 // List(21713170, 47253166, 43710223, 555846477, 815640567, 757804188, 704607002, 66792035, 515489060, 568048905, 418462633, 535030022)
    s.del(418462633)
    s.del(704607002)
    s.del(515489060)
    s.del(66792035)
    s.del(757804188)
    s.sum(158383060, 307743081) // = 0 // 0 // List()
    s.del(535030022)
    s.add(604265055)
    s.sum(21713170, 815640567) // = 2656477563 // 2656477563 // List(21713170, 47253166, 43710223, 555846477, 815640567, 568048905, 604265055)
    s.add(194809808)
    s.add(632079556)
    s.add(163490772)
    s.del(43710223)
    s.sum(4700478, 593159964) // = 1551162298 // 1551162298 // List(21713170, 47253166, 555846477, 568048905, 194809808, 163490772)
    s.add(779397480)
    s.add(558718173)
    s.sum(766613809, 772932257) // = 0 // 0 // List()
    s.sum(21713170, 815640567) // = 4941263129 // 4941263129 // List(21713170, 47253166, 555846477, 815640567, 568048905, 604265055, 194809808, 632079556, 163490772, 779397480, 558718173)
    s.add(861004046)
    s.sum(462550101, 543029166) // = 0 // 0 // List()
    s.add(497060888)
    s.add(738200422)
    s.sum(21713170, 861004046) // = 7037528485 // 7037528485 // List(21713170, 47253166, 555846477, 815640567, 568048905, 604265055, 194809808, 632079556, 163490772, 779397480, 558718173, 861004046, 497060888, 738200422)
    s.sum(21713170, 861004046) // = 7037528485 // 7037528485 // List(21713170, 47253166, 555846477, 815640567, 568048905, 604265055, 194809808, 632079556, 163490772, 779397480, 558718173, 861004046, 497060888, 738200422)
    s.add(498845550)
    s.sum(373064665, 584855100) // = 2678519993 // 2678519993 // List(555846477, 568048905, 558718173, 497060888, 498845550)
    s.del(497060888)
    val ret = s.sum(21713170, 861004046) // = 7037528485 // 7039313147 // List(21713170, 47253166, 555846477, 815640567, 568048905, 604265055, 194809808, 632079556, 163490772, 779397480, 558718173, 861004046, 738200422, 498845550)
    assert(ret == 7039313147L)
  }

  test("case36/2")
  {
//    val s = makeFrom(List(21713170, 47253166, 555846477, 815640567, 568048905, 604265055, 194809808, 632079556, 163490772, 779397480, 558718173, 861004046, 738200422, 498845550))
    val keys = List(21713170, 47253166, 555846477, 815640567, 568048905, 604265055, 194809808, 632079556, 163490772, 779397480, 558718173, 861004046, 497060888, 738200422)
    val s= makeFrom(keys) //, true)
    s.add(498845550)
    s.del(497060888)
    val ret = s.sum(21713170, 861004046) // = 7037528485 // 7039313147 // List(21713170, 47253166, 555846477, 815640567, 568048905, 604265055, 194809808, 632079556, 163490772, 779397480, 558718173, 861004046, 738200422, 498845550)
    assert(ret == 7039313147L)
  }
  test("case36/3")
  {
    var s = new Set
    s + 2 + 3 + 1
    s.root.treeSum
    s + 4
    s - 3
    assert(s.root.treeSum == (2+3+1+4-3))
  }
}
