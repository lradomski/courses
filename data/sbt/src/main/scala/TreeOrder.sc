import scala.collection.mutable

val q = new mutable.Queue[Int]()
q.enqueue(1)
q.enqueue(2)
q.toString()
q.length
q.dequeue()
q.dequeue()