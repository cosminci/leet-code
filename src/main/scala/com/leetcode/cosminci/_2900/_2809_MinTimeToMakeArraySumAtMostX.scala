package com.leetcode.cosminci._2900

import scala.collection.mutable
import scala.util.chaining.*

object _2809_MinTimeToMakeArraySumAtMostX:

  // LeetCode runner bug: fails 100% of the time when submitting, but passes 100% of the time when running the same test cases.
  def minimumTime(nums1: List[Int], nums2: List[Int], x: Int): Int =
    val nums = nums1.zip(nums2).sortBy { case (_, b) => -b }

    val mem = mutable.Map.empty[(Int, Int), Int]
    def dfs(i: Int, j: Int): Int = mem.getOrElseUpdate((i, j),
      if i == nums.length || j == 0 then 0
      else nums(i).pipe { case (a, b) => (a + b * j + dfs(i + 1, j - 1)).max(dfs(i + 1, j)) }
    )

    val (s1, s2) = (nums1.sum, nums2.sum)
    (0 to nums.length).find(k => s1 + s2 * k - dfs(i = 0, j = k) <= x).getOrElse(-1)
