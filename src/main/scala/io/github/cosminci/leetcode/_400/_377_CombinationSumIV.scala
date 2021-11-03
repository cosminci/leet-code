package io.github.cosminci.leetcode._400

import scala.collection.mutable

object _377_CombinationSumIV:
  def main(args: Array[String]): Unit =
    println(combinationSum4TopDown(Array(1, 2, 3), 4))
    println(combinationSum4BottomUp(Array(1, 2, 3), 4))
    println(combinationSum4TopDown(Array(9), 3))
    println(combinationSum4BottomUp(Array(9), 3))

  def combinationSum4TopDown(candidates: Array[Int], target: Int): Int =
    val mem    = mutable.Map.empty[Int, Int]
    val sorted = candidates.sorted

    def dfs(t: Int): Int =
      if t < 0 then return 0
      if t == 0 then return 1

      if mem.contains(t) then return mem(t)

      val result = sorted.map(c => dfs(t - c)).sum
      mem.update(t, result)
      result

    dfs(target)

  def combinationSum4BottomUp(candidates: Array[Int], target: Int): Int =
    val dp = Array.ofDim[Int](target + 1)
    dp(0) = 1
    (1 to target).foreach { t =>
      dp(t) = candidates.map { c =>
        if t >= c then dp(t - c) else 0
      }.sum
    }
    dp.last
