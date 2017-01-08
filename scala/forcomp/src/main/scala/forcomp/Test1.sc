import forcomp.Anagrams.{Occurrences, wordOccurrences,dictionaryByOccurrences,sentenceOccurrences,occSets,combinations,assemble,sentenceAnagrams}

//def combs(o: Occurrences) = combinations(o)
val occ = wordOccurrences("ab") //abcabc")
//dictionaryByOccurrences.contains()
//val so = combinations(sentenceOccurrences(List("Linux","Rulez"))).filter(dictionaryByOccurrences.contains(_))
//so.length
  //)
//val combs = combinations(occ)
//val sets = occSets(occ)
//val ss = assemble(List( List("A1", "A2"), List("B1", "B2")))

//val occ = sentenceOccurrences(List("Linux","Rulez"))
//combinations(occ).length
//sentenceAnagrams(List("Maced"))
sentenceAnagrams(List("Linux", "Rulez"))
