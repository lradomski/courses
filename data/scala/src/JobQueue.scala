import java.io.File
import java.util.Scanner

import scala.collection.mutable

/**
  * Created by luke on 12/3/16.
  */
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

    //      for (i <- 1 to 24)
    //        {
    //          val name = "/Users/luke/git/courses/data/download/Starters PA1/tree_height/tests/%02d".format(i)
    //          val s : Scanner = new Scanner( new File(name) )
    //          val n = s.nextInt();
    //          val jobs = readVertices(s, n)
    //          val out = treeHeight(jobs)
    //
    //          val a = new Scanner( new File(name+".a")).nextInt();
    //          val res = (a==out);
    //          val line = "%02d".format(i)
    //          println(s"${line}: ${res} (${out},${a})");
    //        }

    val s : Scanner = if (args.isEmpty) new Scanner(System.in) else new Scanner( new File(args(0)) )
    val n = s.nextInt();
    val m = s.nextInt();

    val jobs = readLongs(s, m)

    //val jobs : Array[Int] = List(5, 4, 3, 2, 1).toArray

    val threads = jobQueue(n, jobs)

    assert(threads.length == jobs.length)

    for (i <- 0 to jobs.length-1)
    {
      println(threads(i) + " " + jobs(i))

    }


  }

  def jobQueue(n : Int, jobs: Array[Long]) : Array[Int] =
  {
    case class Pending(thread : Int, job : Int, start : Long)


    val threads = new Array[Int](jobs.length) // record of which thread processed which job

    val jobsPending = new mutable.PriorityQueue[Pending]()(Ordering.by(p => -1*(p.start+jobs(p.job))))

    var time : Long = 0

    var i = 0 // job idx

    while (i < n && i < jobs.length)
    {
      jobsPending += (Pending(i, i, time))
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
