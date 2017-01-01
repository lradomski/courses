def invSimple(a: Array[Int]): Int =
{
  var c = 0
  for (i <- 0 to a.length-1)
    for (j <- i to a.length-1)
      if (a(i) > a(j)) c += 1

  c
}

invSimple(Array(2,3,9,2,9))