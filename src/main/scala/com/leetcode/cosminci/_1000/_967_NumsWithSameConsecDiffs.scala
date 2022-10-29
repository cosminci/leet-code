package com.leetcode.cosminci._1000

import scala.collection.mutable

object _967_NumsWithSameConsecDiffs:

  def numsSameConsecDiff(n: Int, k: Int): Array[Int] =
    val mem = mutable.Map.empty[(Int, Int), Set[String]]
    def dfs(pos: Int, prev: Int): Set[String] =
      mem.getOrElseUpdate((pos, prev),
        if pos == n then Set("")
        else Set(prev + k, prev - k)
          .filter(d => d >= 0 && d <= 9)
          .flatMap(d => dfs(pos + 1, d).map(suffix => s"$d$suffix"))
      )

    (1 to 9)
      .flatMap(d => dfs(pos = 1, d).map(suffix => s"$d$suffix"))
      .map(_.toInt).toArray
