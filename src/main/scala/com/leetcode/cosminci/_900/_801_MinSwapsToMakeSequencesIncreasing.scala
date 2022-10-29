package com.leetcode.cosminci._900

import scala.collection.mutable

object _801_MinSwapsToMakeSequencesIncreasing:
  def main(args: Array[String]): Unit =
    println(
      minSwap(
        Array(0, 7, 8, 10, 10, 11, 12, 13, 19, 18),
        Array(4, 4, 5, 7, 11, 14, 15, 16, 17, 20)
      )
    )

  def minSwap(nums1: Array[Int], nums2: Array[Int]): Int =
    val mem = mutable.Map.empty[(Int, Boolean), Int]

    def dfs(idx: Int, prevSwapped: Boolean): Int =
      if idx == nums1.length then return 0
      val (prev1, prev2) = if prevSwapped then (nums2(idx - 1), nums1(idx - 1)) else (nums1(idx - 1), nums2(idx - 1))

      val mustSwap = !(prev1 < nums1(idx)) || !(prev2 < nums2(idx))
      val canSwap  = (prev1 < nums2(idx)) && (prev2 < nums1(idx))
      if mustSwap && !canSwap then return nums1.length

      if mem.contains((idx, prevSwapped)) then return mem((idx, prevSwapped))

      val result =
        if mustSwap then 1 + dfs(idx + 1, prevSwapped = true)
        else if !canSwap then dfs(idx + 1, prevSwapped = false)
        else math.min(dfs(idx + 1, prevSwapped = false), 1 + dfs(idx + 1, prevSwapped = true))

      mem.update((idx, prevSwapped), result)

      result

    math.min(dfs(1, prevSwapped = false), 1 + dfs(1, prevSwapped = true))
