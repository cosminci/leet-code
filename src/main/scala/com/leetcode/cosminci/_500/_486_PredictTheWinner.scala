package com.leetcode.cosminci._500

import scala.collection.mutable

object _486_PredictTheWinner:

  def predictTheWinner(nums: Array[Int]): Boolean =
    val mem = mutable.Map.empty[(Int, Int), Int]
    def dfs(l: Int, r: Int): Int = mem.getOrElseUpdate((l, r),
      if l == r then nums(l)
      else (nums(l) - dfs(l + 1, r)).max(nums(r) - dfs(l, r - 1))
    )
    dfs(l = 0, r = nums.length - 1) >= 0
