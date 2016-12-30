def makeFrom(keys: List[Int]) =
{
  val set = keys.foldLeft(new Set)((set, key) => set + key)
  assert(set.isBinarySearch)
  assert(set.isBalanced)
  set
}

//val keys = List(26,20,40,15,25,30, 50,10,17,29,35)
val keys = List(601030115, 755077338, 579842587, 680116571, 31190791, 248399406, 433103322, 871353531)
val s = makeFrom(keys)
s.sum(31190791, 871353531)


//var s = new Set



//s + 40 + 10 + 50 + 20 + 15 + 17 + 30 + 25 + 26 + 27 + 35
//s.isBalanced
//s + 40 + 30 + 20 + 25 + 15 + 13
//((new Set) + 2 + 1 + 3).isBalanced
//s.height
//s - 13
//s. height
//(new Set).height
//((new Set) + 1 - 1).height
//s.sum(13,40)
//40 + 30 + 20 + 25 + 15 + 13

// test 3
//s + 491572259
//s ? 491572259
//s ? 899375874
//s.s(310971296,877523306) == 491572259
//s + 352411209

// test 2
//s ? 0
//s + 0
//s ? 0
//s - 0
//s ? 0

//s + 2 + 3 + 1
//s.s(1,3)
//val m = (1e9+1).toInt
//(1e9.toInt+3) % m

// test 1
//s ? 1
//s + 1
//s ? 1
//s + 2
//s.s(1,2)
//s + 1000000000
//s ? 1000000000
//s - 1000000000
//s ? 1000000000
//s.s(999999999, 1000000000)
//s - 2
//s ? 2
//s - 0
//s + 9
//s.s(0, 9)