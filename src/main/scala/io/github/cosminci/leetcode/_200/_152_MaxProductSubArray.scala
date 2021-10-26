package io.github.cosminci.leetcode._200

object _152_MaxProductSubArray:
  def main(args: Array[String]): Unit =
    println(maxProduct(Array(2, -5, -2, -4, 3)))
    println(maxProduct(Array(2, 3, -2, 4, -2)))
    println(maxProduct(Array(2, 3, -2, 4, 2)))
    println(maxProduct(Array(-2, 0, -1)))
    println(maxProduct(Array(-2, 0, 1)))

  private def maxProduct(nums: Array[Int]): Int =
    var result                 = nums.max
    var (maxMoving, minMoving) = (1, 1)
    nums.foreach { n =>
      if n == 0 then
        maxMoving = 1
        minMoving = 1
      else {
        val newMaxMoving = Array(minMoving * n, maxMoving * n, n).max
        val newMinMoving = Array(minMoving * n, maxMoving * n, n).min
        maxMoving = newMaxMoving
        minMoving = newMinMoving
        if maxMoving > result then result = maxMoving
      }
    }
    result
