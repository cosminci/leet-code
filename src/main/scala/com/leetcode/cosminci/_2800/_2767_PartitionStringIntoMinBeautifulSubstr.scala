package com.leetcode.cosminci._2800

import scala.util.chaining.*

object _2767_PartitionStringIntoMinBeautifulSubstr:

  def minimumBeautifulSubstrings(s: String): Int =
    val pows = (0 to 6).map(pow => math.pow(5, pow).toInt.toBinaryString).reverse

    def dfs(i: Int): Int =
      if i == s.length then 0
      else pows
        .collect { case pow if s.startsWith(pow, i) => 1 + dfs(i + pow.length) }
        .minOption.getOrElse(s.length * 2)

    dfs(i = 0).pipe(res => if res > s.length then -1 else res)
