package com.leetcode.cosminci._2400

object _2306_NamingACompany:

  def distinctNames(ideas: Array[String]): Long =
    val counts = ideas.groupBy(_.head).view.mapValues(_.map(_.tail).distinct).toMap

    val pairCounts = for
      (prefixA, suffixesA) <- counts
      (prefixB, suffixesB) <- counts
      if prefixA < prefixB
      commonSuffixCount = suffixesA.intersect(suffixesB).length.toLong
    yield (suffixesA.length - commonSuffixCount) * (suffixesB.length - commonSuffixCount)

    pairCounts.sum * 2
