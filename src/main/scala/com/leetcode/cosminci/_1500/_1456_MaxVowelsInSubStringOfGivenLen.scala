package com.leetcode.cosminci._1500

object _1456_MaxVowelsInSubStringOfGivenLen:

  def maxVowels(s: String, k: Int): Int =
    val cnt = s.slice(0, k).count(c => "aeiou".contains(c))
    (k until s.length)
      .foldLeft(cnt, cnt) { case ((maxCnt, cnt), i) =>
        val add    = if "aeiou".contains(s(i)) then 1 else 0
        val remove = if "aeiou".contains(s(i - k)) then 1 else 0
        val newCnt = cnt + add - remove
        (maxCnt.max(newCnt), newCnt)
      }._1
