package com.leetcode.cosminci._100

import scala.collection.mutable

object _40_CombinationSumII:
  def main(args: Array[String]): Unit =
    println(combinationSum2(Array(6), 5))
    println(combinationSum2(Array(10, 1, 2, 7, 6, 1, 5), 8))

  def combinationSum2(candidates: Array[Int], t: Int): List[List[Int]] =
    val sorted = candidates.filter(_ <= t).sorted
    if sorted.sum < t then return List.empty

    val mem = mutable.Map.empty[(Int, Int), List[List[Int]]]
    def dfs(idx: Int, target: Int): List[List[Int]] =
      if idx == sorted.length || sorted(idx) > target then return List.empty
      if sorted(idx) == target then return List(List(idx))
      if mem.contains((idx, target)) then return mem((idx, target))

      val withCurrent    = dfs(idx + 1, target - sorted(idx)).map(_.prepended(idx))
      val withoutCurrent = dfs(idx + 1, target)
      val result         = withCurrent ++ withoutCurrent

      mem.update((idx, target), result)
      result

    dfs(0, t).map(_.map(sorted)).distinct
