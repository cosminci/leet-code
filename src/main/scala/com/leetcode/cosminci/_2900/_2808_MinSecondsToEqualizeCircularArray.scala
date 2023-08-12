package com.leetcode.cosminci._2900

object _2808_MinSecondsToEqualizeCircularArray:

  def minimumSeconds(nums: List[Int]): Int =
    (nums ++ nums).zipWithIndex
      .groupMap { case (n, _) => n } { case (_, i) => i }.values
      .foldLeft(Int.MaxValue)((res, pos) => pos.sliding(2).map(t => (t.last - t.head) / 2).max.min(res))
