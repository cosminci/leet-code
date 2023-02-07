package com.leetcode.cosminci._2600

object _2559_CountVowelStringsInRanges:

  def vowelStrings(words: Array[String], queries: Array[Array[Int]]): Array[Int] =
    val vowels = Set('a', 'e', 'i', 'o', 'u')
    val prefixCount = words.scanLeft(0) { (globalCount, w) =>
      val localCount = if Seq(w.head, w.last).forall(vowels.contains) then 1 else 0
      globalCount + localCount
    }
    queries.map { case Array(l, r) => prefixCount(r + 1) - prefixCount(l) }
