import week4.Substring

val s = Substring

s.findAll("㮒", "")
s.findAllNaive("㮒", "")


s.powx(1)
s.powx(2)
s.powx(3)





s.hash("\01")
s.hash("\01\02")
s.hash("\02\01")


val q1 = s.hash(0, 0, 1, 10)
val q2 = s.hash(q1, 0, 2, 10)
val q3 = s.hash(q2, 1, 1, 10)
val q4 = s.hash(q3, 2, 2, 10)

"0123".substring(1,3)
for (i <- 1 to 0) yield i



val x = 10
val powX = 100
val p = 101

def h(n: Int) = n % p

h(123)
h(234)


//val c1 = (h(123) % powX) * x + 4
//h(c1)

val t1 = (h(123) + p) % powX
h(t1)

val t2 = (h(123) + p) / powX
val t3 = (h(123)+p - (t2*powX)%p)
h(t3)

val t4 = h(h(t1) * x + 4)
val t5 = h(h(h(t1) * x) + 4)

//h(t2)

//32


//for (i <- -1 to 0 by -1) yield i
//val a = Array(0, 5, -1, 2)
//val h = new Heap(a)(_>_)
//a.toList
//for (i <- 1 to a.length) yield h.takeTop
//Heap.sortInPlace(Array(0, 5, -1, 2)).sorted.toList
//Heap.sort(Array(0, 1, -1, 2)).toList