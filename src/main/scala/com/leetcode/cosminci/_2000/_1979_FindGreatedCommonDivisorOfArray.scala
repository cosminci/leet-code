package com.leetcode.cosminci._2000

import com.leetcode.cosminci.utils.gcd

object _1979_FindGreatedCommonDivisorOfArray:
  def main(args: Array[String]): Unit =
    println(findGCD(Array(2, 5)))

  def findGCD(nums: Array[Int]): Int =
    gcd(nums.min, nums.max)
