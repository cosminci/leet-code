package io.github.cosminci.leetcode._200

object _136_SingleNumber:
  def singleNumber(nums: Array[Int]): Int =
    nums.reduce(_ ^ _)
