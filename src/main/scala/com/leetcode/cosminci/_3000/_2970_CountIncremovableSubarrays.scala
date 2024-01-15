package com.leetcode.cosminci._3000

object _2970_CountIncremovableSubarrays:

  def incremovableSubarrayCount(nums: Array[Int]): Int =
    (0 to nums.length).combinations(2).count { case Seq(i, j) =>
      (nums.slice(0, i) ++ nums.slice(j, nums.length))
        .sliding(2)
        .forall(g => g.length == 1 || g(0) < g(1))
    }
