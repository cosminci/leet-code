package com.leetcode.cosminci._1100

import scala.collection.mutable

object _1035_UncrossedLines:

  def maxUncrossedLines(nums1: Array[Int], nums2: Array[Int]): Int =
    val mem = mutable.Map.empty[(Int, Int), Int]

    def dfs(i: Int, j: Int): Int = mem.getOrElseUpdate((i, j),
      if i == nums1.length || j == nums2.length then 0
      else if nums1(i) != nums2(j) then dfs(i + 1, j).max(dfs(i, j + 1))
      else dfs(i + 1, j).max(dfs(i, j + 1)).max(1 + dfs(i + 1, j + 1))
    )

    dfs(i = 0, j = 0)
