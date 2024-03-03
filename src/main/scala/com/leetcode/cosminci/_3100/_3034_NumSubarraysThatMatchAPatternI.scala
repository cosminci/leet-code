package com.leetcode.cosminci._3100

import scala.collection.mutable

object _3034_NumSubarraysThatMatchAPatternI:

  def countMatchingSubarrays(nums: Array[Int], pattern: Array[Int]): Int =
    def matches(i: Int, j: Int) =
      pattern(j) == 0 && nums(i) == nums(i + 1) ||
        pattern(j) == 1 && nums(i) < nums(i + 1) ||
        pattern(j) == -1 && nums(i) > nums(i + 1)

    val mem = mutable.Map.empty[(Int, Int), Int]
    def dfs(i: Int, j: Int): Int = mem.getOrElseUpdate((i, j),
      if j == pattern.length then 1
      else if i == nums.length - 1 then 0
      else if matches(i, j) then dfs(i + 1, j + 1) else 0
    )

    nums.indices.map(i => dfs(i, j = 0)).sum
