import Rope.{CharOffset, NodePos, EmptyNodePos, OffsetRange, Tree, Set}
import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Prop.forAll
import org.scalatest.FunSuite
import org.scalatest.prop.Checkers

class RopeTest
extends FunSuite with Checkers
{
  test("OffsetRange")
  {
    val or = OffsetRange(-2,3)
    val aor = or.toAbs(2)
    assert(aor == OffsetRange(0,5))
    val ror = aor.toRel(2)
    assert(or == ror)

    val max = 300*1000
    val ranges =
          for {
          l <- Gen.choose(-max,max)
          r <- Gen.choose(l, l+max)
        } yield (l,r)
    val gen = ranges suchThat (lr => lr._1 <= lr._2) // Arbitrary.arbitrary[(Int,Int)]


    check
    {
      forAll(gen)
      {
        case (l,r) =>
          {
            assert(l <= r)
            val or = OffsetRange(l,r)
            val pos = -1*l
            val aor = or.toAbs(pos)
            val test = OffsetRange(0, r-l)

            aor == test  &&  test.toRel(pos) == or
          }
      }
    }

  }

  test("NodePos-setParent")
  {
    val c = new Tree(null, null, null, CharOffset('a', 0))
    val p = new Tree(null, null, null, CharOffset('b', 1))

    val cnp = c.withPos(0)
    assert(cnp.absPos == c.key.off)
    val cPos = cnp.absPos
    assert(cnp.hasNotParent)
    assert(cnp.hasNotLeft)
    assert(cnp.hasNotRight)
    assert(cnp.isRoot)


    val pnp = p.withPos(0)
    assert(pnp.absPos == p.key.off)
    val pPos = pnp.absPos
    assert(pnp.hasNotParent)
    assert(pnp.hasNotLeft)
    assert(pnp.hasNotRight)
    assert(pnp.isRoot)


    cnp.setParent(pnp)
    assert(cnp.absPos == cPos)
    assert(c.key.off == cPos - pPos)
    assert(cnp.hasParent)
    assert(!cnp.isRoot)


    cnp.setParent(EmptyNodePos)
    assert(cnp.hasNotParent)
    assert(cnp.absPos == cPos)
    assert(c.key.off == cPos)
    assert(cnp.isRoot)

  }

  test("NodePos-isNull")
  {
    val c = new Tree(null, null, null, CharOffset('a', 0))
    val cnp = c.withPos(0)
    assert(cnp.isNotNull)
    assert(EmptyNodePos.isNull)
  }

  test("NodePos-setLeft,setRight,adjustHeight")
  {
    val c = new Tree(null, null, null, CharOffset('a', 0))
    val p = new Tree(null, null, null, CharOffset('b', 1))
    val c2 = new Tree(null, null, null, CharOffset('c', 2))

    val cnp = c.withPos(0)
    assert(cnp.n.range == OffsetRange(0,0))

    val pnp = p.withPos(0)
    val pPos = pnp.absPos
    assert(pnp.n.range == OffsetRange(0,0))
    assert(pnp.hasNotLeft)


    pnp.setLeft(cnp)
    cnp.setParent(pnp)
    assert(pnp.hasLeft)
    assert(pnp.absPos == pPos)
    pnp.adjustHeight
    assert(pnp.n.height == 2)
    assert(pnp.n.range == OffsetRange(-1,0))
    assert(cnp.n.range == OffsetRange(0,0))

    pnp.setLeft(EmptyNodePos)
    cnp.setParent(EmptyNodePos)
    assert(pnp.hasNotLeft)
    assert(pnp.absPos == pPos)
    pnp.adjustHeight
    assert(pnp.n.height == 1)
    assert(pnp.n.range == OffsetRange(0,0))
    assert(cnp.n.range == OffsetRange(0,0))

    val c2np = c2.withPos(0)
    assert(c2np.n.range == OffsetRange(0,0))

    pnp.setRight(c2np)
    c2np.setParent(pnp)
    assert(pnp.hasRight)
    assert(pnp.absPos == pPos)
    pnp.adjustHeight
    assert(pnp.n.height == 2)
    assert(pnp.n.range == OffsetRange(0,1))
    assert(c2np.n.range == OffsetRange(0,0))

    pnp.setRight(EmptyNodePos)
    c2np.setParent(EmptyNodePos)
    assert(pnp.hasNotRight)
    assert(pnp.absPos == pPos)
    pnp.adjustHeight
    assert(pnp.n.height == 1)
    assert(pnp.n.range == OffsetRange(0,0))

    pnp.setLeft(cnp);
    cnp.setParent(pnp)
    pnp.setRight(c2np)
    c2np.setParent(pnp)
    pnp.adjustHeight
    assert(pnp.n.height == 2)
    assert(pnp.n.range == OffsetRange(-1,1))
  }

  test("Tree-find")
  {
    val l = new Tree(null, null, null, CharOffset('a', 0))
    val p = new Tree(null, null, null, CharOffset('b', 1))
    val r = new Tree(null, null, null, CharOffset('c', 2))


    val pnp = p.withPos(0)
    pnp.setLeft(l.withPos(0).setParent(pnp)).setRight(r.withPos(0).setParent(pnp))
    pnp.adjustHeight

    val s = new Set
    s.root = p

    {
      val n = s.find(0)
      assert(n.n.key.char == 'a')
      assert(n.absPos == 0)
    }

    {
      val n = s.find(1)
      assert(n.n.key.char == 'b')
      assert(n.absPos == 1)
    }

    {
      val n = s.find(2)
      assert(n.n.key.char == 'c')
      assert(n.absPos == 2)
    }
  }


  test("Tree-next")
  {
    val l = new Tree(null, null, null, CharOffset('a', 0))
    val p = new Tree(null, null, null, CharOffset('b', 1))
    val r = new Tree(null, null, null, CharOffset('c', 2))


    val pnp = p.withPos(0)
    pnp.setLeft(l.withPos(0).setParent(pnp)).setRight(r.withPos(0).setParent(pnp))
    pnp.adjustHeight

    val s = new Set
    s.root = p

    val a = s.find(0)
    assert(a.n.key.char == 'a')
    assert(a.absPos == 0)

    val b = s.next(a)

    {
      val n = b
      assert(n.n.key.char == 'b')
      assert(n.absPos == 1)
    }

    val c = s.next(b)

    {
      val n = c
      assert(n.n.key.char == 'c')
      assert(n.absPos == 2)
    }
  }

  test("Tree-rebalance/left")
  {
    val a = new Tree(null, null, null, CharOffset('a', 0))
    val b = new Tree(null, null, null, CharOffset('b', 1))
    val c = new Tree(null, null, null, CharOffset('c', 2))


    val anp = a.withPos(0)
    val bnp = b.withPos(0)
    val cnp = c.withPos(0)

    anp.setRight( bnp.setParent(anp).setRight( cnp.setParent(bnp)) )
    bnp.adjustHeight

    assert(anp.n.height == 3)
    assert(bnp.n.height == 2)
    assert(cnp.n.height == 1)
    assert(anp.n.range == OffsetRange(0, 2))
    assert(bnp.n.range == OffsetRange(0, 1))
    assert(cnp.n.range == OffsetRange(0, 0))

    val s = new Set
    s.root = anp.n

    s.rebalance(anp)
    assert(s.root == bnp.n)

    assert(anp.n.height == 1)
    assert(bnp.n.height == 2)
    assert(cnp.n.height == 1)
    assert(anp.n.range == OffsetRange(0, 0))
    assert(bnp.n.range == OffsetRange(-1, 1))
    assert(cnp.n.range == OffsetRange(0, 0))
  }

  test("Tree-rebalance/right")
  {
    val a = new Tree(null, null, null, CharOffset('a', 0))
    val b = new Tree(null, null, null, CharOffset('b', 1))
    val c = new Tree(null, null, null, CharOffset('c', 2))


    val anp = a.withPos(0)
    val bnp = b.withPos(0)
    val cnp = c.withPos(0)

    cnp.setLeft( bnp.setParent(cnp).setLeft( anp.setParent(bnp)) )
    bnp.adjustHeight

    assert(cnp.n.height == 3)
    assert(bnp.n.height == 2)
    assert(anp.n.height == 1)
    assert(cnp.n.range == OffsetRange(-2, 0))
    assert(bnp.n.range == OffsetRange(-1, 0))
    assert(anp.n.range == OffsetRange(0, 0))

    val s = new Set
    s.root = cnp.n

    s.rebalance(cnp)
    assert(s.root == bnp.n)

    assert(anp.n.height == 1)
    assert(bnp.n.height == 2)
    assert(cnp.n.height == 1)
    assert(anp.n.range == OffsetRange(0, 0))
    assert(bnp.n.range == OffsetRange(-1, 1))
    assert(cnp.n.range == OffsetRange(0, 0))
  }

  test("Tree-add/del/text")
  {
    val s = new Set

    val in = "abc"

    in.foldLeft(0)((i,ch) => { s.add(ch, i); i+1 })

    val out = s.chars

    val ok = (in zip out).forall(lr => lr._1==lr._2)
    assert(ok)

    val ch = in(1)
    s.del(s.find(1))
    val ok2 = (in.filter(_!=ch) zip s.chars).forall(lr => lr._1==lr._2)
    assert(ok2)
  }

  test("Tree-split")
  {
    val s = new Set

    val in = "abcde"
    in.foldLeft(0)((i,ch) => { s.add(ch, i); i+1 })

    val idx = 2
    val (before, after) = s.split(idx)

    assert(before.text == in.substring(0,idx+1))

    assert(after.text == in.substring(idx+1))


  }
}
