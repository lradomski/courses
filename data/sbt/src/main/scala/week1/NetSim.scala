import java.io.File
import java.util.Scanner

import scala.collection.immutable.Queue
import scala.collection.mutable

object NetSim
{

  case class Packet(arrived: Int, duration: Int, var started: Int)

  def simulate(packets: Array[Packet], bufferSize: Int): Unit =
  {
    var in = packets.foldLeft(mutable.Queue[Packet]())((q, p) => {q.enqueue(p); q})
    var q = mutable.Queue[Packet]()
    var time = 0

    while (!in.isEmpty || !q.isEmpty)
    {
      var times = List[Int]()

      if (!q.isEmpty) // anything to process ?
      {
        val p = q.head
        if (-1 == p.started) p.started = time //start first one if not done yet - first is pending one

        val end = (p.started + p.duration)
        if (end == time) q.dequeue // time to finish ?

        times = end :: times // for time advancing
      }

      if (!in.isEmpty)
      {
        times = in.head.arrived :: times // for time advancing

        if (in.head.arrived == time) // did anything arrive ?
        {
          val p = in.dequeue

          if (q.size < bufferSize) q.enqueue(p)
          else assert(p.started == -1)
        }
      }

      // advance time - next incoming packet or processing completion point, whichever is earlier
      if (!times.isEmpty) time = times.min
    }
  }

  def mainAll(args: Array[String]): Unit =
  {
        for (i <- 1 to 22)
          {
            println(">>>>>     " + i)
            val name = "/Users/luke/git/courses/data/download/Starters PA1/network_packet_processing_simulation/tests/%02d".format(i)
            val s : Scanner = new Scanner( new File(name) )

            val bufferSize = s.nextInt
            val n = s.nextInt

            val packets = (for (i <- 1 to n) yield Packet(s.nextInt, s.nextInt, -1)).toArray

            simulate(packets, bufferSize)

            val a = new Scanner( new File(name+".a"))
            packets.foldLeft(0)(
              (l,p) =>
                {
                  val test = a.nextInt
                  //assert(p.started == test, l + ": " + p.started + "!=" + test)
                  val res = (p.started==test);
                  val line = "%02d".format(i)
                  if (!res) println(s"${line}: ${res} (${p.started},${test})");
                  l + 1
                }
            )
          }

  }

  def main(args: Array[String]): Unit =
  {
    val s = new Scanner(System.in)

    val bufferSize = s.nextInt
    val n = s.nextInt

    val packets = (for (i <- 1 to n) yield Packet(s.nextInt, s.nextInt, -1)).toArray

    simulate(packets, bufferSize)

    packets.foreach(p => println(p.started))
  }

}
