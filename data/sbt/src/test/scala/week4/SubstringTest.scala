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
    check((text: String, patter: String) =>
    {
      if (!text.isEmpty && !patter.isEmpty)
      {

        val o1 = Substring.findAll(text, patter)
        val o2 = Substring.findAllNaive(text, patter)
        o1._2 == o2._2 && (o1._1.slice(0, o1._2) zip o2._1.slice(0, o2._2)).forall(lr => lr._1 == lr._2)
      }
      else true
    })
  }
}
