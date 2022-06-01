package io.github.cosminci.leetcode._1500

object _1480_RunningSumOf1dArray:

  def runningSum(nums: Array[Int]): Array[Int] =
    nums.scanLeft(0)(_ + _).tail
