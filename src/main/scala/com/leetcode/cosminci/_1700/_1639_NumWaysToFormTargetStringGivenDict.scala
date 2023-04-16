package com.leetcode.cosminci._1700

import scala.collection.mutable

object _1639_NumWaysToFormTargetStringGivenDict:

  def numWays(words: Array[String], target: String): Int =
    val (m, n) = (words.head.length, target.length)
    val counts = Array.tabulate(m)(i => words.map(_(i)).groupMapReduce(identity)(_ => 1)(_ + _).withDefaultValue(0))

    val mem = mutable.Map.empty[(Int, Int), Long]
    def dfs(i: Int, j: Int): Long = mem.getOrElseUpdate((i, j),
      if i == n then 1
      else if j == m then 0
      else (dfs(i, j + 1) + dfs(i + 1, j + 1) * counts(j)(target(i))) % 1_000_000_007
    )

    dfs(i = 0, j = 0).toInt
