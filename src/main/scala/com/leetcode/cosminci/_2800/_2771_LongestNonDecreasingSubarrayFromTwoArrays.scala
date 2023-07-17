package com.leetcode.cosminci._2800

import scala.collection.mutable

object _2771_LongestNonDecreasingSubarrayFromTwoArrays:

  def maxNonDecreasingLength(nums1: Array[Int], nums2: Array[Int]): Int =
    val mem = mutable.Map.empty[(Int, Int), Int]
    def dfs(i: Int, prevArr: Int): Int = mem.getOrElseUpdate((i, prevArr),
      if i == nums1.length then 0
      else if prevArr == 0 then
        dfs(i + 1, prevArr = 0).max(1 + dfs(i + 1, prevArr = 1)).max(1 + dfs(i + 1, prevArr = 2))
      else
        val prevTarget = if prevArr == 1 then nums1(i - 1) else nums2(i - 1)
        Seq((nums1(i), 1), (nums2(i), 2))
          .collect { case (n, arr) if n >= prevTarget => 1 + dfs(i + 1, prevArr = arr) }
          .maxOption.getOrElse(0)
    )

    dfs(i = 0, prevArr = 0)
