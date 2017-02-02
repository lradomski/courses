import org.scalatest.FunSuite
import org.scalatest.prop.Checkers

/**
  * Created by luke on 1/30/17.
  */
class SubstringTest
  extends FunSuite with Checkers
{
  //val cond: (l: Int, r: Int) => l>r
  test("substring")
  {
    check((text: String, pattern: String) =>
    {
      if (!text.isEmpty && !pattern.isEmpty)
      {

        val o1 = Substring.findAll(text, pattern)
        val o2 = Substring.findAllNaive(text, pattern)
        o1._2 == o2._2 && (o1._1.slice(0, o1._2) zip o2._1.slice(0, o2._2)).forall(lr => lr._1 == lr._2)
      }
      else true
    })
  }
}
