package io.github.cosminci.leetcode._100

import scala.collection.mutable

object _78_Subsets:
  def subsets(nums: Array[Int]): List[List[Int]] =
    val mem = mutable.Map.empty[Int, List[List[Int]]]

    def dfs(idx: Int): List[List[Int]] =
      if idx == nums.length then return List(List.empty)
      if mem.contains(idx) then return mem(idx)

      val nextIdxSubsets = dfs(idx + 1)
      val result         = nextIdxSubsets ++ nextIdxSubsets.map(_.prepended(nums(idx)))

      mem.update(idx, result)
      result

    dfs(0)
