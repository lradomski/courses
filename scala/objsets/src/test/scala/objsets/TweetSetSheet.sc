import objsets._

val t2 = new Tweet("2", "2", 2)
val t1 = new Tweet("1", "1", 1)

val ts2 = new NonEmpty(t2, new Empty, new Empty)
val ts1 = new NonEmpty(t1, new Empty, new Empty)
ts2.union(ts1).foreach(t => println(t))
