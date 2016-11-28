fiboHugeMod(2816213588L, 30524)

def fiboHugeMod(n: Long, mod: Int) : Int =
{
  require(n > 0, "n > 0")
  require(n <= 10e18, "n <= 10e18")
  require(mod > 1, "mod > 1")
  require(mod <= 10e5, "10e5")

  def fibo(n : Long) : Int = fiboMod(n, mod)

  var i : Long = 0
  var period : Long = 0

  do
  {
    while(0 != fibo(i) && 1 != fibo(i+1)) i+=1

    i+=2
    period = i

    while (i < 3*period && fibo(i-period) == fibo(i)) i+=1
  } while (! (i == 3*period))


  fibo(n % period)
}

def fiboMod(n: Long, mod : Int): Int = {
  require(n >= 0, "n >= 0")
  require(n <= 10*1000*1000, "n <= 10*1000*1000")

  val f = new Array[Int](3)

  def idx(i : Long) : Int = (i % f.length).toInt


  var i : Long = 0
  while  (i <= n)
  {
    if (i <= 1) f( idx(i) ) = i.toInt
    else {
      f( idx(i) ) = ( f( idx(i - 1) ) + f( idx(i - 2)) ) % mod
    }
    i = i + 1;
  }

  f(idx(n))
}

def fibonacci3(n: Long): Int = {
  require(n >= 0, "n >= 0")
  require(n <= 10*1000*1000, "n <= 10*1000*1000")

  val f = new Array[Int](3)

  def idx(i : Long) : Int = (i % f.length).toInt


  var i : Long = 0
  while  (i <= n)
  {
    if (i <= 1) f( idx(i) ) = i.toInt
    else {
      f( idx(i) ) = ( f( idx(i - 1) ) + f( idx(i - 2)) ) % 10
    }
    i = i + 1;
  }

  f(idx(n))
}

def fibonacci2(n: Int): Int = {
  require(n >= 0, "n >= 0")
  require(n <= 45, "n <= 45")

  val f = new Array[Int](3)

  for (i <- 0 to n) {
    //println(i)
    if (i <= 1) f(i % 3) = i
    else {
      //println((i % 3) + " = " + ((i-1) % 3) + ", " + ((i-2) % 3) )
      f(i % 3) = f((i - 1) % 3) + f((i - 2) % 3)
    }
  }

  f(n % 3)
}

def fibonacci(n: Int): Int = {
  require(n >= 0, "n >= 0")
  require(n <= 45, "n <= 45")

  val f = new Array[Int](n + 1)
  f(n) = 0


  for (i <- 0 to n) {
    if (i <= 1) f(i) = i
    else f(i) = f(i - 1) + f(i - 2)
  }

  f(n)
}


