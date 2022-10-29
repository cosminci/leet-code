package com.leetcode.cosminci._500

object _413_ArithmeticSlices {
  def main(args: Array[String]): Unit = {
    println(numberOfArithmeticSlices(Array(1)))
    println(numberOfArithmeticSlices(Array(1, 2, 3, 4, 5, 8, 9, 11)))
  }

  def numberOfArithmeticSlices(nums: Array[Int]): Int =
    (2 until nums.length).scanLeft(0) {
      case (length, idx) =>
        if (nums(idx) - nums(idx - 1) == nums(idx - 1) - nums(idx - 2)) length + 1 else 0
    }.sum
}
