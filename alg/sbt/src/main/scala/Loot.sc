import java.text.DecimalFormat

case class Item(value: Int, weight: Int) //extends Ordered[Item]
{
  def valuePerUnit: Double = value.toDouble / weight
}

val formatter = new DecimalFormat("#.####")
var items = Array( Item(60,20), Item (100,50), Item(120,30) )
items = items.sortWith((l, r) => l.valuePerUnit > r.valuePerUnit)
items.foreach(i => println(i.toString))

