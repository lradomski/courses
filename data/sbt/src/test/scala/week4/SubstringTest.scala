package week4

import org.scalatest.FunSuite
import org.scalatest.prop.Checkers

/**
  * Created by luke on 1/30/17.
  */
class SubstringTest
extends FunSuite with Checkers
  {
    //val cond: (l: Int, r: Int) => l>r
    test("heap sort - new array")
    {
      check((main: String, sub: String) =>
        {
          val o1 = Substring.findAll(main, sub)
          val o2 = Substring.findAllNaive(main,sub)
          o1.length == o2.length && (o1 zip o2).forall(lr => lr._1 == lr._2)
        })
    }
}
