package com.leetcode.cosminci._2300

object _2256_MinAvgDiff:

  def main(args: Array[String]): Unit =
    println(minimumAverageDifference(Array(0)))

  def minimumAverageDifference(nums: Array[Int]): Int = {
    val prefixSum = nums.scanLeft(0L)(_ + _).tail
    val suffixSum = nums.scanRight(0L)(_ + _).tail
    nums.indices.minBy { i =>
      val prefixAvg = prefixSum(i) / (i + 1)
      val suffixAvg = if (i == nums.length - 1) 0 else suffixSum(i) / (nums.length - 1 - i)
      (prefixAvg - suffixAvg).abs
    }
  }
