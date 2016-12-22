import PhoneBook.Contact

import scala.collection.mutable

val l = new mutable.MutableList[Contact]()
l += new Contact(1, "A1")
l.+:(new Contact(1001, "A1001"))
//l +: new Contact(1001, "A1001")
l//.filter(c => c.number == 1001)
//l += new Contact(2, "A2")
