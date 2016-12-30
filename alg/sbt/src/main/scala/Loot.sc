import java.text.DecimalFormat

import Loot.{Item, loot}

val formatter = new DecimalFormat("#.####")
def sort(l: Item, r: Item) = l.valuePerUnit > r.valuePerUnit
//items.foreach(i => println(i.toString))

formatter.format(loot(50, Array( Item(60,20), Item (100,50), Item(120,30) ).sortWith(sort)))

formatter.format(loot(10, Array(Item(500,30)).sortWith(sort)))