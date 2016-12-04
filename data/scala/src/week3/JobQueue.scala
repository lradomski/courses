import java.io.File
import java.util.Scanner

object JobQueue
{

  def main(args: Array[String]) =
  {

    def readInts(s: Scanner, n: Int): Array[Int] = {
      val tree = new Array[Int](n)
      for (i <- 0 to n - 1) {
        tree(i) = s.nextInt()
      }
      tree
    }

    def readLongs(s: Scanner, n: Int): Array[Long] = {
      val tree = new Array[Long](n)
      for (i <- 0 to n - 1) {
        tree(i) = s.nextLong()
      }
      tree
    }

    val s : Scanner = if (args.isEmpty) new Scanner(System.in) else new Scanner( new File(args(0)) )
    val n = s.nextInt();
    val m = s.nextInt();

    val jobs = readLongs(s, m)

    //val jobs : Array[Long] = List(5, 4, 3, 2, 1).toArray

    val threads = jobQueue(n, jobs)

    assert(threads.length == jobs.length)

    if (args.isEmpty)
    {
      for (i <- 0 to jobs.length-1)
      {
        println(threads(i) + " " + jobs(i))
      }
    }
    else
    {
      val s2 = new Scanner( new File(args(0)+".a"))

      var i = 0
      while (i < jobs.length && s2.hasNext)
        {
          val t = s2.nextInt()
          val s = s2.nextLong()
          val ok = if (threads(i) == t && jobs(i) == s) "  " else "> "
          println(ok + threads(i) + " " + jobs(i) + " / " + t + " " + s)
          assert(threads(i) == t, "threads(i) == t")
          assert(jobs(i) == s, "jobs(i) == s")
          i += 1
        }
    }



  }

  def jobQueue(n : Int, jobs: Array[Long]) : Array[Int] =
  {
    case class Pending(thread : Int, job : Int, start : Long) extends Ordered[Pending]
    {
      override def compare(that: Pending): Int =
      {
        if (this.end == that.end) -1*this.thread.compare(that.thread)
        else -1*this.end.compare(that.end)
      }

      def end = start+jobs(job)
    }


    val threads = new Array[Int](jobs.length) // record of which thread processed which job

    val jobsPending = new scala.collection.mutable.PriorityQueue[Pending]() //(Ordering.by(p => -1*(p.start+jobs(p.job))))

    var time : Long = 0

    var i = 0 // job idx

    while (i < n && i < jobs.length)
    {
      jobsPending += Pending(i, i, time)
      i += 1
    }

    while (!jobsPending.isEmpty)
    {
      val done = jobsPending.dequeue()

      time = done.start + jobs(done.job)
      threads(done.job) = done.thread
      jobs(done.job) = done.start

      if (i < jobs.length)
      {
        jobsPending += Pending(done.thread, i, time)
        i+=1
      }
    }


    threads
  }


}
