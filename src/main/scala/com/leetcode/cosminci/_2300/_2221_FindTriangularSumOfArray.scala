package com.leetcode.cosminci._2300

object _2221_FindTriangularSumOfArray:
  def triangularSum(nums: Array[Int]): Int =
    val binomialCoefficients = nums.indices.scanLeft(BigInt(1))((mCk, k) => mCk * (nums.length - 1 - k) / (k + 1))
    nums.zip(binomialCoefficients).foldLeft(BigInt(0)) { case (sum, (n, mCk)) => (sum + n * mCk) % 10 }.toInt
