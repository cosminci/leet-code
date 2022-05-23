package io.github.cosminci.leetcode._500

import scala.collection.mutable

object _474_OnesAndZeroes:

  def findMaxForm(strs: Array[String], m: Int, n: Int): Int =
    def oneZeroCounts(s: String): (Int, Int) =
      s.foldLeft(0, 0) { case ((ones, zeroes), char) =>
        if char == '1' then (ones + 1, zeroes) else (ones, zeroes + 1)
      }

    val sorted = strs.map(oneZeroCounts).sortBy { case (ones, zeroes) => -(ones + zeroes) }

    val mem = mutable.Map.empty[(Int, Int, Int), Int]
    def dfs(i: Int, m: Int, n: Int): Int = mem.getOrElseUpdate((i, m, n),
      if i == strs.length then 0
      else
        val (ones, zeroes) = sorted(i)
        if ones > n || zeroes > m then dfs(i + 1, m, n)
        else dfs(i + 1, m, n).max(1 + dfs(i + 1, m - zeroes, n - ones))
    )

    dfs(i = 0, m, n)
