package com.leetcode.cosminci._2700

object _2681_PowerOfHeroes:

  def sumOfPower(nums: Array[Int]): Int =
    val mod = 1_000_000_007
    nums.sortInPlace()
    nums.foldLeft(0L, 0L) { case ((res, s), n) =>
      ((res + (s + n) * n % mod * n % mod) % mod, (s * 2 + n) % mod)
    }._1.toInt
