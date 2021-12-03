package io.github.cosminci.leetcode._200

object _152_MaxProductSubArray:
  def main(args: Array[String]): Unit =
    println(maxProduct(Array(2, -5, -2, -4, 3)))
    println(maxProduct(Array(2, 3, -2, 4, -2)))
    println(maxProduct(Array(2, 3, -2, 4, 2)))
    println(maxProduct(Array(-2, 0, -1)))
    println(maxProduct(Array(-2, 0, 1)))

  def maxProduct(nums: Array[Int]): Int =
    nums
      .foldLeft(nums.max, 1, 1) { case ((result, maxMoving, minMoving), n) =>
        if n == 0 then (result, 1, 1)
        else
          val newMaxMoving = (minMoving * n).max(maxMoving * n).max(n)
          val newMinMoving = (minMoving * n).min(maxMoving * n).min(n)
          (result.max(newMaxMoving), newMaxMoving, newMinMoving)
      }
      ._1
