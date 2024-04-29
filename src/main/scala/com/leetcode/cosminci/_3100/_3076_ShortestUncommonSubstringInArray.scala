package com.leetcode.cosminci._3100

object _3076_ShortestUncommonSubstringInArray:

  def shortestSubstrings(arr: Array[String]): Array[String] =
    arr.zipWithIndex.map { case (s, i) =>
      (0 to s.length).combinations(2)
        .map { case Seq(j, k) => s.slice(j, k) }
        .distinct.toList
        .sortBy(ss => (ss.length, ss))
        .find(ss => arr.zipWithIndex.forall { case (s, j) => i == j || !s.contains(ss) })
        .getOrElse("")
    }
