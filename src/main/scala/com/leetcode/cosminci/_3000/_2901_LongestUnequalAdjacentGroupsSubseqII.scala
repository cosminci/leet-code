package com.leetcode.cosminci._3000

import scala.collection.mutable

object _2901_LongestUnequalAdjacentGroupsSubseqII:

  def getWordsInLongestSubsequence(n: Int, words: Array[String], groups: Array[Int]): List[String] =
    def isValid(i: Int, j: Int): Boolean =
      if groups(i) == groups(j) then false
      else if words(i).length != words(j).length then false
      else words(i).zip(words(j)).count { case (c1, c2) => c1 != c2 } == 1

    val mem = mutable.Map.empty[(Int, Int), Seq[String]]
    def dfs(prev: Int, i: Int): Seq[String] = mem.getOrElseUpdate((prev, i),
      if i == n then Seq.empty
      else
        val pick = if prev == -1 || isValid(prev, i) then words(i) +: dfs(i, i + 1) else Seq.empty
        val skip = dfs(prev, i + 1)
        if pick.length > skip.length then pick else skip
    )

    dfs(prev = -1, i = 0).toList
