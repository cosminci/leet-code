package com.leetcode.cosminci._500

import scala.collection.mutable

object _491_IncreasingSubsequences:

  def findSubsequences(nums: Array[Int]): List[List[Int]] =
    val mem = mutable.Map.empty[(Int, Int), List[List[Int]]]
    def dfs(i: Int, prev: Int): List[List[Int]] = mem.getOrElseUpdate((i, prev),
      if i == nums.length then List(List.empty)
      else if nums(i) < prev then dfs(i + 1, prev)
      else dfs(i + 1, nums(i)).map(nums(i) +: _) ++ dfs(i + 1, prev)
    )

    val subsequences = for
      i <- nums.indices
      j <- i + 1 until nums.length
      if nums(i) <= nums(j)
    yield dfs(j + 1, nums(j)).map(List(nums(i), nums(j)) ++: _)

    subsequences.flatten.distinct.toList
