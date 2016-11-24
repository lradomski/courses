package week1

import java.io.{File, InputStream}
import java.util.Scanner


object CheckBrackets
{
  def main(args: Array[String])
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
    val s : Scanner = if (args.isEmpty) new Scanner(System.in) else new Scanner( new File(args(0)) )

    val text = s.nextLine()
    val out = checkBrackets(text)

    if (args.isEmpty) println(out) else println(new Scanner( new File(args(0)+".a")).nextLine()==out );
  }

  def checkBrackets(text : String) : String =
  {
    val emptyChar = '\0'


    def check(chars : List[Char], openPosClose : List[(Int,Char)]) : List[(Int,Char)] =
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

      if (chars.isEmpty) openPosClose
      else
      {
        val close = closeForOpen(chars.head)
        val isOpen = close != emptyChar;
        if (isOpen) check(chars.tail, (chars.length, close) :: openPosClose)
        else
        {
          if (isClose(chars.head))
          {
            if (openPosClose.isEmpty) (chars.length, chars.head) :: openPosClose // close with no open
            else if (chars.head == openPosClose.head._2) check(chars.tail, openPosClose.tail) // right close
            else (chars.length, chars.head) :: openPosClose // wrong type close
          }
          else check(chars.tail, openPosClose)

        }
      }
    }

    val chars = text.toList;
    val r = check(chars, List[(Int,Char)]())

    if (r.isEmpty) "Success" else (chars.length - r.head._1 + 1).toString
  }

}
