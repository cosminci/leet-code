package com.leetcode.cosminci._100

import scala.collection.mutable

object _39_CombinationSum:
  def main(args: Array[String]): Unit =
    println(combinationSum(Array(2, 3, 6, 7), 7))
    println(combinationSum(Array(2, 3, 5), 8))
    println(combinationSum(Array(2), 1))
    println(combinationSum(Array(1), 1))
    println(combinationSum(Array(1), 2))

  def combinationSum(nums: Array[Int], target: Int): List[List[Int]] =
    nums.sortInPlace()

    val mem = mutable.Map.empty[(Int, Int), List[List[Int]]]
    def dfs(t: Int, idx: Int): List[List[Int]] =
      mem.getOrElseUpdate((t, idx), {
        if idx == nums.length || t < nums(idx) then List.empty
        else if t == nums(idx) then List(List(t))
        else dfs(t - nums(idx), idx).map(nums(idx) +: _) ++ dfs(t, idx + 1)
      })

    dfs(target, idx = 0)
