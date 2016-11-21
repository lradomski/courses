

def balance2(chars: List[Char]): Boolean =
{
  def isParentheses(c : Char) = c=='(' || c==')'

  def expr(from : Int) : Int =
  {
    val left = chars.indexWhere(isParentheses, from);
    if (-1 != left)
    {
      if (chars(left) == '(')
      {
        val core = expr(left+1);
        val right = chars.indexOf(')', core) ;
        if (-1 != right) right+1 else chars.length;
      }
      else from;
    }
    else left;
  }

  var lastExp = 0;
  var newExp = 0;

  do {
    lastExp = newExp;
    newExp = expr(lastExp)
  } while (newExp > lastExp && newExp < chars.length)


  newExp == chars.length
}

def balance3(chars: List[Char]): Boolean =
{
  def isParentheses(c : Char) = c=='(' || c==')'

  def subExpr(from : Int) : Int =
  {
    val left = chars.indexWhere(isParentheses, from);
    if (-1 != left)
    {
      if (chars(left) == '(')
      {
        val core = expr(left+1);
        val right = chars.indexOf(')', core) ;
        if (-1 != right) right+1 else -1;
      }
      else from;
    }
    else chars.length;
  }


  def expr(from: Int) : Int =
  {
    var prev = -1;
    var curr = from;

    while (prev < curr && curr < chars.length)
      {
        prev = curr;
        curr = subExpr(curr);
      }

    curr
  }

  val end = expr(0);
  end == chars.length
}

def balance4(chars: List[Char]): Boolean =
{
  def isParentheses(c : Char) = c=='(' || c==')'

  def subExpr(chars: List[Char]) : (List[Char],Boolean) =
  {
    //print("+subExpr(" + outer); println(":" + chars);

    def core(chars: List[Char]) : (List[Char],Boolean)  =
    {
      //print("+core(" + outer); println(":" + chars);

      if (chars.isEmpty) return (chars, false)

      if (chars.head == '(') expr(chars)
      else if (chars.head == ')') (chars.tail, true)
      else core(chars.tail)
    }

    if (chars.isEmpty) (chars, true)

    if (chars.head == '(') core(chars.tail)
    else if (chars.head == ')') (chars, true)
    else subExpr(chars.tail)
  }


  def expr(chars: List[Char]) =
  {
    //print("+Expr(" + outer); println(":" + chars);
    var result : (List[Char],Boolean) = (chars, true)

    do
    {
      result = subExpr(result._1);
    } while (result._2 && !result._1.isEmpty)

    //print("-Expr"); println(":" + result);
    result
  }

  val result = expr(chars)
  result._2 && result._1.isEmpty
}

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
//"(()())".toList.indexOf('A')

balance("()()".toList)
balance("(()())".toList)
//balance("()".toList)
//"123".toList.tail

balance("(if (zero? x) max (/ 1 x))".toList)
balance("I told him (that it's not (yet) done).\n(But he wasn't listening)".toList)
!balance(":-)".toList)
!balance("())(".toList)
!balance(")()".toList)