import java.util.regex.Pattern

//val ns = List(1,2,3)
//val ls = List('a','b','c')
//
//ls.flatMap(l => ns.map(n => (n,l))).groupBy(_._1).mapValues(v => v.length)

//List(1,2,3).sortBy(-1*_)

//val lang = "Java"
//val p = Pattern.compile("\\b" + lang + "\\b")
val p = "(?i)\\bJava\\b".r.pattern
p.matcher("JavaScript").find()
p.matcher(" Java Script").find()
p.matcher(" java Script").find()
//"Java, C++".split(Array(' ')).map(s => "[" + s + "]").toList