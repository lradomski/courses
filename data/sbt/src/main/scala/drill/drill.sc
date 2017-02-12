

trait Generator[+T]
{
  self =>

  def generate: T

  def map[U](f: T => U): Generator[U] = new Generator[U]
  {
    override def generate = f(self.generate)
  }

  def flatMap[U](f: T => Generator[U]): Generator[U] = new Generator[U] {
    override def generate: U = f(self.generate).generate
  }
}

val integers = new Generator[Int] {
  val rand = new java.util.Random

  def generate = rand.nextInt(100)
}

val booleans = for (i <- integers) yield i > 0

def pairs[T,U](t: Generator[T], u: Generator[U]) = for
  {
    i <- t
    j <- u
  } yield (i,j)


def single[T](t: T) = new Generator[T]
{
  override def generate: T = t
}

def choose(lo: Int, hi: Int) = for {i <- integers} yield lo + i % (hi+1-lo)

//choose(0, 2).generate

def oneOf[T](xs: T*) = for (i <- choose(0, xs.length)) yield xs(i)

def chances(winning: Int, total: Int) = for { i <- choose(0, total) } yield i <= winning

val NullNode: drill.Node[Int] = null

def tree(min: Int, max: Int, depth: Int, maxDepth: Int): Generator[drill.Node[Int]] = for
{
  i <- choose(min, max)
  hasLeft <- chances(depth, maxDepth)
  l <- if (hasLeft && min < i-1) tree(min, i-1, depth-1, maxDepth) else single(NullNode)
  hasRight <- chances(depth, maxDepth)
  r <- if (hasRight && i+1 < max) tree(i+1, max, depth-1, maxDepth) else single(NullNode)
} yield new drill.Node[Int](l, r, null, i)

//def node = oneOf(single(NullNode), tree)
//for {
//  i <- 1 to 10
//  n <- chances(i, 10)
//} yield n

//(1 to 10).flatMap(i => chances(i, 10))

//(for
//{
//  //i <- 10 to 0 by -1
//  ok <- chances(5, 10)
//  //if (ok)
//} yield ok)//.toList
  //yield i)//.toList
  //yield (i,chances(i,10).generate)).toList

//(for { i <- choose(0,10)} yield i).generate

tree(0, 100, 5, 5).generate

//type Node = drill.Node[Int]
//
//val r = new Node(new Node(null, null, null, 0), new Node(null, null, null, 2), null, 1)
//r.left.toString
//r.right.toString
//r.toString