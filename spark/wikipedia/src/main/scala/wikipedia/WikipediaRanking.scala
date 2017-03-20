package wikipedia



import java.util.regex.Pattern

import org.apache.log4j.{Level, Logger}
import org.apache.spark.SparkConf
import org.apache.spark.SparkContext
import org.apache.spark.SparkContext._
import org.apache.spark.rdd.RDD

case class WikipediaArticle(title: String, text: String)

object WikipediaRanking {

  val langs = List(
    "JavaScript", "Java", "PHP", "Python", "C#", "C++", "Ruby", "CSS",
    "Objective-C", "Perl", "Scala", "Haskell", "MATLAB", "Clojure", "Groovy")

//  val langPats = langs.map(l => (l, l.r.pattern)).toMap
//  def hasLang(p: Pattern, a: WikipediaArticle) = p.matcher(a.text).find()
//  def wordPattern(s: String) = ("(?i)\\b" + s + "\\b").r.pattern


  val conf: SparkConf = new SparkConf().setAppName("wikipedia").setMaster("local[*]")
  val sc: SparkContext = new SparkContext(conf)
  val rootLogger = Logger.getRootLogger()
  rootLogger.setLevel(Level.WARN)

  // Hint: use a combination of `sc.textFile`, `WikipediaData.filePath` and `WikipediaData.parse`
  val wikiRdd: RDD[WikipediaArticle] = sc.textFile(WikipediaData.filePath).map(WikipediaData.parse(_)).persist()

  /** Returns the number of articles on which the language `lang` occurs.
   *  Hint1: consider using method `aggregate` on RDD[T].
   *  Hint2: should you count the "Java" language when you see "JavaScript"?
   *  Hint3: the only whitespaces are blanks " "
   *  Hint4: no need to search in the title :)
   */

  def occurrencesOfLang(lang: String, rdd: RDD[WikipediaArticle]): Int =
    {
      //val pattern = wordPattern(lang)

      rdd.aggregate(0)(
//        (count, article) => count + (if (pattern.matcher(article.text).find()) 1 else 0),
        (count, article) => count + (if (article.text.split(' ').contains(lang)) 1 else 0),
        _ + _
      )
    }

  def occurrencesOfLang2(lang: String, rdd: Array[WikipediaArticle]): Int =
  {
    //val pattern = wordPattern(lang)

    rdd.aggregate(0)(
//      (count, article) => count + (if (pattern.matcher(article.text).find()) 1 else 0),
      (count, article) => count + (if (article.text.split(' ').contains(lang)) 1 else 0),
      _ + _
    )
  }

  /* (1) Use `occurrencesOfLang` to compute the ranking of the languages
   *     (`val langs`) by determining the number of Wikipedia articles that
   *     mention each language at least once. Don't forget to sort the
   *     languages by their occurrence, in decreasing order!
   *
   *   Note: this operation is long-running. It can potentially run for
   *   several seconds.
   */
  def rankLangs(langs: List[String], rdd: RDD[WikipediaArticle]): List[(String, Int)] =
    langs.map(lang => (lang,occurrencesOfLang(lang,rdd))).sortBy(-1*_._2)

  def rankLangs2(langs: List[String], rdd: Array[WikipediaArticle]): List[(String, Int)] =
    langs.map(lang => (lang,occurrencesOfLang2(lang,rdd))).sortBy(-1*_._2)

  /* Compute an inverted index of the set of articles, mapping each language
   * to the Wikipedia pages in which it occurs.
   */
  def makeIndex(langs: List[String], rdd: RDD[WikipediaArticle]): RDD[(String, Iterable[WikipediaArticle])] =
  {
//    val langPats = langs.map(l => (l, wordPattern(l))).toMap
//    def hasLang(p: Pattern, a: WikipediaArticle) = p.matcher(a.text).find()
//
//    rdd.flatMap(
//      a => langPats.filter( langPat => hasLang(langPat._2, a)).map(langPat => (langPat._1,a))
//    ).groupByKey()

    rdd.flatMap(
      a => langs.filter( a.text.split(' ').contains(_)).map((_,a))
    ).groupByKey()
  }

  /* (2) Compute the language ranking again, but now using the inverted index. Can you notice
   *     a performance improvement?
   *
   *   Note: this operation is long-running. It can potentially run for
   *   several seconds.
   */
  def rankLangsUsingIndex(index: RDD[(String, Iterable[WikipediaArticle])]): List[(String, Int)] =
    index.map(las => (las._1,las._2.size)).collect().sortBy(-1*_._2).toList

  /* (3) Use `reduceByKey` so that the computation of the index and the ranking are combined.
   *     Can you notice an improvement in performance compared to measuring *both* the computation of the index
   *     and the computation of the ranking? If so, can you think of a reason?
   *
   *   Note: this operation is long-running. It can potentially run for
   *   several seconds.
   */
  def rankLangsReduceByKey(langs: List[String], rdd: RDD[WikipediaArticle]): List[(String, Int)] =
    {
//      val langPats = langs.map(l => (l, wordPattern(l))).toMap
//      def hasLang(p: Pattern, a: WikipediaArticle) = p.matcher(a.text).find()
//
//      rdd.flatMap(
//        a => langPats.filter( langPat => hasLang(langPat._2, a)).map(langPat => (langPat._1,1))
//      ).reduceByKey(_ + _).collect().sortBy(-1*_._2).toList

      rdd.flatMap(
              a => langs.filter( a.text.split(' ').contains(_)).map((_,1))
            ).reduceByKey(_ + _).collect().sortBy(-1*_._2).toList
    }

  def main(args: Array[String]) {

//    for (i <- 1 to 10) println
//    timed("count", wikiRdd.count())
//    timed("count2", wikiRdd.collect().length)
//    val langsRankedArr: List[(String, Int)] = timed("Part 1: naive ranking (arr)", rankLangs2(langs, wikiRdd.collect()))
//    timed("", wikiRdd.map(
//      a => (a, wordPattern("Java").matcher(a.text).find(), a.text.split(" ").contains("Java"))
//    ).filter(arc => arc._2 != arc._3).take(10).toList)

    /* Languages ranked according to (1) */
    val langsRanked: List[(String, Int)] = timed("Part 1: naive ranking", rankLangs(langs, wikiRdd))
//    val langsRanked1a: List[(String, Int)] = timed("Part 1a: naive ranking", rankLangs(langs, wikiRdd))

    /* An inverted index mapping languages to wikipedia pages on which they appear */
    def index: RDD[(String, Iterable[WikipediaArticle])] = makeIndex(langs, wikiRdd)

    /* Languages ranked according to (2), using the inverted index */
    val langsRanked2: List[(String, Int)] = timed("Part 2: ranking using inverted index", rankLangsUsingIndex(index))

    /* Languages ranked according to (3) */
    val langsRanked3: List[(String, Int)] = timed("Part 3: ranking using reduceByKey", rankLangsReduceByKey(langs, wikiRdd))

    /* Output the speed of each ranking */
    println(timing)

//    println(langsRanked.map(_._2).sum)
//    println(langsRanked2.map(_._2).sum)
//    println(langsRanked3.map(_._2).sum)

    sc.stop()
  }

  val timing = new StringBuffer
  def timed[T](label: String, code: => T): T = {
//    println
//    println(" >>>>> " + label)

    val start = System.currentTimeMillis()
    val result = code
    val stop = System.currentTimeMillis()
    timing.append(s"Processing $label took ${stop - start} ms.\n")

//    println(result)
//    println(s"//Processing $label took ${stop - start} ms.\n")

    result
  }
}
