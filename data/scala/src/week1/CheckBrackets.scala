import java.io.{File, InputStream}
import java.util.Scanner

import scala.collection.mutable


object CheckBrackets
{
  def main(args: Array[String]) =
  {
//    for (i <- 1 to 54)
//      {
//        val name = "/Users/luke/git/courses/data/download/Starters PA1/check_brackets_in_code/tests/%02d".format(i)
//        val s : Scanner = new Scanner( new File(name) )
//
//        val text = s.nextLine()
//        val out = checkBrackets(text)
//
//        val a = new Scanner( new File(name+".a")).nextLine();
//        val res = (a==out);
//        val line = "%02d".format(i)
//        println(s"${line}: ${res} (${out},${a})");
//      }

    //val t0 = System.nanoTime();
    val s : Scanner = if (args.isEmpty) new Scanner(System.in) else new Scanner( new File(args(0)) )

    val text = s.nextLine()
    //val t1 = System.nanoTime();
    //println( ((t1-t0)/(1000*1000)).toString );
    val out = checkBrackets(text)
    //val t2 = System.nanoTime();
    //println( ((t2-t1)/(1000*1000)).toString );

    if (args.isEmpty) println(out) else println(new Scanner( new File(args(0)+".a")).nextLine()==out );
  }


  def checkBrackets(text : String) : String =
  {
    val emptyChar = '\u0000'


    def check(chars : Array[Char]) : Int =
    {

      def closeForOpen(c : Char) : Char =
      {
        if (c == '(') ')'
        else if (c == '[') ']'
        else if (c == '{') '}'
        else emptyChar
      }

      def isClose(c : Char) : Boolean =
      {
        if (c == ')' || c == ']' || c == '}') true else false
      }

      val openPos : mutable.Stack[Int] = mutable.Stack[Int]()


      for(pos <- 0 to chars.length-1)
      {
        val c = chars(pos);
        if ('(' == c || '[' == c || '{' == c)  // is open ?
        {
          openPos.push(pos)
        }
        else
        {
          if (')' == c || ']' == c || '}' == c) // is close ?
          {
            if (openPos.isEmpty) return pos // close with no open
            else if (c == closeForOpen(chars(openPos.head))) // right close
              {
                openPos.pop();
              }
            else return pos // wrong type close
          }
          else
            {
              // just skip - it's a non-paren
            }
        }
      }

      if (openPos.isEmpty) return chars.length else openPos.head;
    }

    //val t0 = System.nanoTime();
    val chars = text.toCharArray;
    //val t1 = System.nanoTime();
    //println( ((t1-t0)/(1000*1000)).toString );

    val r = check(chars)
    //val t2 = System.nanoTime();
    //println( ((t2-t1)/(1000*1000)).toString );

    if (r==chars.length) "Success" else (r+1).toString
  }

}
