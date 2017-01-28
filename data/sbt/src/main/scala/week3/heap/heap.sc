import week3.Heap

//for (i <- -1 to 0 by -1) yield i
val a = Array(0, 5, -1, 2)
val h = new Heap(a)(_>_)
//a.toList
//for (i <- 1 to a.length) yield h.takeTop
//Heap.sortInPlace(Array(0, 5, -1, 2)).sorted.toList
//Heap.sort(Array(0, 1, -1, 2)).toList