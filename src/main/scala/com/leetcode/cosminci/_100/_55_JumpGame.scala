package com.leetcode.cosminci._100

object _55_JumpGame:

  def main(args: Array[String]): Unit =
    println(canJump(Array(0, 2, 3)))
    println(canJump(Array(2, 3, 1, 1, 4)))
    println(canJump(Array(3, 2, 1, 0, 4)))

  def canJump(nums: Array[Int]): Boolean =
    nums.indices.foldLeft(0) { case (max, i) =>
      if (max < i) return false
      math.max(max, i + nums(i))
    } >= nums.length - 1
