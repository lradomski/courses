import java.io.File
import java.util.Scanner

import scala.collection.mutable

object HashChains
{

  class Cmd(val cmd: String, val arg: String)
  {
    override def toString: String = cmd + ": " + arg
  }

  class HashTable(val m: Int)
  {
    val strings = new Array[mutable.MutableList[String]](m)

    private val p = 1000000007L
    private val x = 263

    def hash(s: String): Int =
    {
      var h: Long = 0
      var xPower : Long = 1
      for (c <- s)
      {
        h += c*xPower
        xPower *= x
      }

      ((h % p) % m).toInt
    }

    def add(s: String): Unit =
    {
      val i = hash(s)
      if (null == strings(i)) strings(i) = new mutable.MutableList[String]()
      val chain = strings(i)
      val existing = chain.find(chained => chained == s).getOrElse(null)

      if (null == existing) strings(i) = s +: chain
    }

    def del(s: String): Unit =
    {
      val i = hash(s)
      val chain = strings(i)
      if (null != chain)
      {
        strings(i) = chain.filterNot(chained => chained == s)
      }

    }

    def find(s: String): Boolean =
    {
      val i = hash(s)
      val chain = strings(i)
      if (null != chain) !chain.forall(chained => chained != s)
      else false
    }

    def check(i: Int): mutable.Iterable[String] =
    {
      val chain = strings(i)
      if (null == chain) mutable.MutableList[String]() else chain
    }
  }

  def main(args: Array[String]) =
  {
    val s: Scanner = if (args.isEmpty) new Scanner(System.in) else new Scanner(new File(args(0)))

    val m = s.nextInt()
    val strings = new HashTable(m)
    var n = s.nextLong()


    def readCmd: Cmd = new Cmd(s.next(), s.next())

    while (n > 0)
    {
      n -= 1

      val cmd = readCmd

      if (cmd.cmd == "add") strings.add(cmd.arg)
      else if (cmd.cmd == "del") strings.del(cmd.arg)
      else if (cmd.cmd == "find") println(if (strings.find(cmd.arg)) "yes" else "no")
      else if (cmd.cmd == "check")
      {
        strings.check(cmd.arg.toInt).foreach(s => print(s + " "))
        println()
      }
    }


  }

}
