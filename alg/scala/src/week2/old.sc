fibonacci(45)


def fibonacci(n : Int) : Int =
{
  require(n >= 0, "n >= 0")
  require(n <= 45, "n <= 45")

  val f = new Array[Int](n+1)


  for (i <- 0 to n)
  {
    if (n <= 1) f(i) = i
    else f(i) = f(i-1) + f(i-2)
  }

  f(n)
}

def fibonacci2(n : Int) : Int =
{
  require(n >= 0, "n >= 0")
  require(n <= 45, "n <= 45")

  val f = new Array[Int](3)


  for (i <- 0 to n)
  {
    if (n <= 1) f(i % 3) = i
    else f(i) = f((i-1) % 3) + f( (i-2) % 3)
  }

  f(n)
}
