package com.leetcode.cosminci._400

object _396_RotateFunction:
  def main(args: Array[String]): Unit =
    println(maxRotateFunction(Array(4, 3, 2, 6)))

  def maxRotateFunction(nums: Array[Int]): Int =
    val (total, n) = (nums.sum, nums.length)
    val initialSum = nums.indices.foldLeft(0) { case (sum, i) =>
      sum + i * nums(i)
    }
    (1 until nums.length)
      .scanLeft(initialSum) { case (prevSum, i) =>
        prevSum + total - n * nums(n - i)
      }
      .max
