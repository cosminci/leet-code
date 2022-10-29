package com.leetcode.cosminci._100

object _41_FirstMissingPositive:
  def main(args: Array[String]): Unit =
    println(firstMissingPositive(Array(1, 2, 0)))
    println(firstMissingPositive(Array(3, 4, -1, 1)))
    println(firstMissingPositive(Array(7, 8, 9, 11, 12)))

  def firstMissingPositive(nums: Array[Int]): Int =
    nums.indices.foreach { i => if nums(i) < 0 then nums(i) = 0 }
    nums.indices.foreach { i =>
      val j = math.abs(nums(i)) - 1
      if j >= 0 && j < nums.length then
        if nums(j) > 0 then nums(j) = -nums(j)
        else if nums(j) == 0 then nums(j) = -(nums.length + 1)
    }
    (1 to nums.length).find(i => nums(i - 1) >= 0) match
      case Some(n) => n
      case None    => nums.length + 1
