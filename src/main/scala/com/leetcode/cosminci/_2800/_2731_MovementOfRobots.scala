package com.leetcode.cosminci._2800

object _2731_MovementOfRobots:

  def sumDistance(nums: Array[Int], s: String, d: Int): Int =
    val mod = 1_000_000_007

    nums.indices.zip(s).foreach {
      case i -> 'L' => nums(i) -= d
      case i -> 'R' => nums(i) += d
    }

    nums.sortInPlace()
    val pSum = nums.scanLeft(0L) { case (x, y) => (x + y) % mod }

    (1 until nums.length)
      .foldLeft(0L)((res, i) => (res + i.toLong * nums(i) - pSum(i)) % mod)
      .toInt
