import java.text.DecimalFormat
import java.util.Scanner

object Loot
{

  def main(args: Array[String]) =
  {
    val formatter = new DecimalFormat("#.####")
    val s = new Scanner(System.in);

    case class Item(value: Int, weight: Int) //extends Ordered[Item]
    {
      def valuePerUnit: Double = value.toDouble / weight

      override def toString: String = value.toString + " " + weight.toString + " (" + formatter.format(valuePerUnit) + ")"

      //      override def compare(that: Item): Int =
      //      {
      //        valuePerUnit.compare(that.valuePerUnit)
      //      }
    }

    def readItemsSort(n: Int): Array[Item] =
    {
      var items = new Array[Item](n)
      for (i <- 0 to n - 1)
      {
        items(i) = Item(s.nextInt(), s.nextInt)

      }

      items = items.sortWith((l, r) => l.valuePerUnit > r.valuePerUnit)
      items
    }

    def min(l: Int, r: Int) = if (l > r) r else l

    def loot(capacity: Int, items: Array[Item]): Double =
    {
      var remaining = capacity
      var value: Double = 0.0

      var i = 0

      while (0 < remaining && i < items.length)
      {
        val item = items(i)
        i += 1

        if (item.weight >= remaining) // more item than we can carry - take what we can
        {
          value += remaining * item.valuePerUnit
          remaining = 0
        }
        else // take entire item
        {
          value += item.value
          remaining -= item.weight
        }
      }

      value
    }


    val nItems = s.nextInt()
    val capacity = s.nextInt()
    val items = readItemsSort(nItems)

//    val nItems = 3
//    val capacity = 50
//    var items = Array( Item(60,20), Item (100,50), Item(120,30) )
//    items = items.sortWith((l, r) => l.valuePerUnit > r.valuePerUnit)
    val a = loot(capacity, items)
    println(formatter.format(a))
  }


}
