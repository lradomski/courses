import scala.collection.mutable


val j : Array[Long] = Array(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1)
  //Array(1,2,3,4,5)
val t  = jobQueue(4, j)
assert(j.length == t.length)
for (i <- 0 to j.length-1) println(t(i) + " " + j(i))

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