"ABC".toCharArray().length

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

  if (r.isEmpty) "Success" else (chars.length - r.head._1).toString
}

checkBrackets("([{([{}{}][])()}{}{}][()])")