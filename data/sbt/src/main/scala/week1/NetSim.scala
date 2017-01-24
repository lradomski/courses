import java.util.Scanner

import scala.collection.immutable.Queue

object NetSim
{

  case class Packet(arrival: Int, duration: Int, var processed: Int)

  def main(args: Array[String]): Unit =
  {
    val s = new Scanner(System.in)

    val bufferSize = s.nextInt
    val n = s.nextInt

    val packets = (for (i <- 1 to n) yield Packet(s.nextInt, s.nextInt, -1)).toArray
    //var remaining = packets

    var processing = false
    var q = Queue[Packet]()
    var time = 0

    var i = 0

    while (i < packets.length || !q.isEmpty)
    {
      if (q.isEmpty) // nothing to work on
      {
        //processing = false
        if (i < packets.length) // advance time to next packet
          {
            assert(time <= packets(i).arrival)
            time = packets(i).arrival
          }
      }
      else
      {
        val pq = q.dequeue
        val p = pq._1
        p.processed = time
        time += p.duration
        q = pq._2
      }

      while (i < packets.length && packets(i).arrival <= time)
      {
        if (q.size < bufferSize) q = q.enqueue(packets(i))
        else packets(i).processed = -1

//        if (processing)
//        {
//        }
//        else
//        {
//          val p = packets(i)
//          p.processed = time
//          time += p.duration
//          processing = true
//        }

        i += 1
      }
    }

    packets.foreach(p => println(p.processed))


    /*
    3

    0 3 -> 0

    1 1
    1 1
    1 1
    1 1


     */

  }

}
