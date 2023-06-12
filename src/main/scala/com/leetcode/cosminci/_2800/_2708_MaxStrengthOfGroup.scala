package com.leetcode.cosminci._2800

object _2708_MaxStrengthOfGroup:

  def maxStrength(nums: Array[Int]): Long =
    nums.sortInPlace()
    val (res, cnt) = nums.indices.foldLeft(1L, 0) { case ((res, cnt), i) =>
      if res * nums(i) > 0 || (i + 1 < nums.length && nums(i + 1) < 0) then (res * nums(i), cnt + 1) else (res, cnt)
    }
    if cnt > 0 then res else nums.last
