import java.util.Scanner


object Substring
{
    val p = 100019
    val x = 256

//  val p = 101
//  val x = 10

  def hp(n: Long) = (n % p).toInt

  def hp(n: Int) = n % p

  def powx(n: Int): Int =
  {
    var pow = 1
    for (i <- 1 to n) pow = hp(pow.toLong * x)
    pow
  }


  def hash(s: String): Int =
  {
    s.foldLeft(0)((h, c) =>
      hp(hp(h.toLong * x) + c)
    )
  }

  def hash(h: Int, out: Char, in: Char, powX: Int): Int =
  {
    /*
      txtHash = (txtHash + q - RM*txt.charAt(i-m) % q) % q;
      txtHash = (txtHash*R + txt.charAt(i)) % q;
     */
    val hNoOut = hp(h + p - hp(out.toLong * powX))
    val newHash = hp((hNoOut.toLong * x) + in)
    newHash.toInt
  }


  def main(args: Array[String]) =
  {
    val s = new Scanner(System.in)
    val pattern = s.next
    val text = s.next

    assert(text.length > 0)
    assert(pattern.length > 0)


    val oc = findAll(text, pattern)
    for (i <- 0 until oc._2) print(oc._1(i) + " ")
  }

  def findAllNaive(text: String, pattern: String): (Array[Int], Int) =
  {
    val occs = new Array[Int](text.length)
    var io = 0

    if (text.length >= pattern.length)
      {
        for (i <- 0 to text.length-pattern.length)
          if (text.substring(i, i+pattern.length) == pattern)
            {
              occs(io) = i
              io += 1
            }
      }

    (occs,io)
  }

  def findAll(text: String, pattern: String): (Array[Int], Int) =
  {
    val occs = new Array[Int](text.length)
    var io = 0


    if (text.isEmpty || pattern.isEmpty)
      {}
    else if (text.length == pattern.length)
    {
      //if (hash(str) == hash(sub) && str == sub) println(0)
      if (text == pattern)
      {
        occs(io) = 0
        io += 1
      }
    }
    else if (text.length > pattern.length)
    {
      val xPowN = powx(pattern.length - 1)
      val hsub = hash(pattern)

      var hstr = hash(text.substring(0, pattern.length))
      if (hstr == hsub && text.substring(0, pattern.length) == pattern)
        {
          occs(io) = 0
          io += 1
        }

      //println(">>> " + hsub)
      //println(0 + "> " + str.substring(0, 0 + sub.length) + " > " + hstr)

      for (i <- 1 to text.length - pattern.length)
      {
        hstr = hash(hstr, text(i - 1), text(i + pattern.length - 1), xPowN)
        if (hstr == hsub && text.substring(i, i + pattern.length) == pattern)
          {
            occs(io) = i
            io += 1
          }

        //println(i + "> " + str.substring(i, i + sub.length) + " > " + hstr)
      }
    }

    (occs,io)
  }
}
