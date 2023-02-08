package com.leetcode.cosminci._100

import scala.collection.mutable

object _45_JumpGameII:

  def jump(nums: Array[Int]): Int =
    val mem = mutable.Map.empty[Int, Int]
    def dfs(i: Int): Int = mem.getOrElseUpdate(i,
      if i == nums.length - 1 then 0
      else if nums(i) == 0 then Int.MaxValue
      else 1 + ((i + 1) to (i + nums(i)).min(nums.length - 1)).map(dfs).min
    )
    dfs(i = 0)
