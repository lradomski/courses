import java.io.File
import java.util.Scanner

import scala.collection.mutable

object PhoneBook
{

  class Contact(val number: Long, var name: String)
  {
    override def toString: String = number.toString + "/" + name
  }

  class Cmd(val cmd: String, val contact: Contact)
  {
    override def toString: String = cmd + ":" + contact.toString
  }


  def main(args: Array[String]) =
  {
    val s: Scanner = if (args.isEmpty) new Scanner(System.in) else new Scanner(new File(args(0)))

    def readCmd: Cmd =
    {
      val cmd = s.next()
      if (cmd == "del") new Cmd(cmd, new Contact(s.nextLong(), ""))
      else if (cmd == "find") new Cmd(cmd, new Contact(s.nextLong(), ""))
      else if (cmd == "add") new Cmd(cmd, new Contact(s.nextLong(), s.next()))
      else null
    }

    var n = s.nextLong();
    val contacts = new Array[mutable.MutableList[Contact]](1000)
    val p = 1009
    assert(contacts.length < p, "numbers.length < p")

    def hash(n: Long): Int = ((n % p) % contacts.length).toInt

    def add(contact: Contact): Unit =
    {
      val i = hash(contact.number)
      if (null == contacts(i)) contacts(i) = new mutable.MutableList[Contact]()
      val chain = contacts(i)
      val old = chain.find(c => c.number == contact.number).getOrElse(null)

      if (null == old) contacts(i) += contact else old.name = contact.name
    }

    def find(number: Long): Contact =
    {
      val i = hash(number)
      val chain = contacts(i)
      if (null == chain) null
      else
      {
        chain.find(c => c.number == number).getOrElse(null)
      }
    }

    def del(number: Long): Unit =
    {
      val i = hash(number)
      val chain = contacts(i)
      if (null != chain)
      {
        contacts(i) = chain.filterNot(c => c.number == number)
      }
    }

    while (n > 0)
    {
      n -= 1
      val c = readCmd
      if (c.cmd == "add")
      {
        add(c.contact)
      }
      else if (c.cmd == "find")
      {
        val contact = find(c.contact.number)
        if (null == contact) println("not found")
        else println(contact.name)
      }
      else if (c.cmd == "del")
      {
        del(c.contact.number)
      }
      else
      {

      }
    }


  }

}
