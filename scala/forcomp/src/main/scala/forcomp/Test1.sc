import forcomp.Anagrams.{Occurrences, wordOccurrences,dictionaryByOccurrences,sentenceOccurrences,occSets,combinations,assemble,sentenceAnagrams}

//def combs(o: Occurrences) = combinations(o)
//val occ = wordOccurrences("ab") //abcabc")
//dictionaryByOccurrences.contains()
//val so = combinations(sentenceOccurrences(List("Linux","Rulez"))).filter(dictionaryByOccurrences.contains(_))
//so.length
  //)
//val combs = combinations(occ)
//val sets = occSets(occ)
//val ss = assemble(List( List("A1", "A2"), List("B1", "B2")))

val occ = sentenceAnagrams(List("Linux","Rulez")).map(_.reduceLeft(_+" "+_))
occ.forall(c=> occ.filter(c2 => c == c2).length == 1)

val occ2 = sentenceAnagrams("Heather")
occ2.forall(c=> occ2.filter(c2 => c == c2).length == 1)
occ2.foreach(c=>
{
  val len = occ2.filter(c2 => c == c2).length
  if (len != 1) println(len + ": " + c)
})
//combinations(occ).length
//sentenceAnagrams(List("Maced"))
//sentenceAnagrams("Luke Radomski")
