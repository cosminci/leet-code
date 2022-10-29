package com.leetcode.cosminci._1500

object _1413_MinValueToGetPositiveStepByStepSum {
  def minStartValue(nums: Array[Int]): Int =
    1 - nums.scanLeft(0)(_ + _).min
}
