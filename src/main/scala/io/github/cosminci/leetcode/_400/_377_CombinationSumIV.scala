package io.github.cosminci.leetcode._400

import scala.collection.mutable

object _377_CombinationSumIV:

  def combinationSum4(candidates: Array[Int], target: Int): Int =
    val mem = mutable.Map.empty[Int, Int]
    def dfs(t: Int): Int = mem.getOrElseUpdate(t,
      if t < 0 then 0
      else if t == 0 then 1
      else candidates.map(c => dfs(t - c)).sum
    )
    dfs(target)
