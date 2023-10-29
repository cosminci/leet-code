package com.leetcode.cosminci._2900

object _2900_LongestUnequalAdjacentGroupsSubseqI:

  def getWordsInLongestSubsequence(n: Int, words: Array[String], groups: Array[Int]): List[String] =
    (groups :+ Int.MaxValue)
      .sliding(2)
      .zipWithIndex
      .collect { case (Array(g1, g2), i) if g1 != g2 => words(i) }
      .toList
