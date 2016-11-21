package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int =
  {
    if (0 == c && r == 0) 1 else
    if (r < 0) 0 else
    if (c < 0 || c > r+1) 0 else
      pascal(c-1, r-1) + pascal(c, r-1);
  }
  
  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean =
  {
    def core(chars: List[Char], count: Int) : Int =
    {
      //println(chars + "/ " + count)

      if (chars.isEmpty) count
      else if (count < 0) count
      else if (chars.head == '(') core(chars.tail, count+1)
      else if (chars.head == ')') core(chars.tail,count-1)
      else core(chars.tail, count)
    }

    //println(chars)
    core(chars, 0) == 0
  }



  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = -1
  }
