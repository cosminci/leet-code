package com.leetcode.cosminci._2500

object _2501_LongestSquareStreakInArray:

  def longestSquareStreak(nums: Array[Int]): Int =
    nums.sorted
      .foldLeft(Map.empty[Int, Int]) { (powers, n) =>
        val sqrt = math.sqrt(n).toInt
        if sqrt * sqrt != n || !powers.contains(sqrt) then powers.updated(n, 1)
        else powers.updated(n, powers(sqrt) + 1)
      }
      .collect { case (_, streak) if streak >= 2 => streak }
      .maxOption
      .getOrElse(-1)
