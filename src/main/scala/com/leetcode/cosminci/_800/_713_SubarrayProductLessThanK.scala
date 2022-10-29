package com.leetcode.cosminci._800

object _713_SubarrayProductLessThanK:
  def main(args: Array[String]): Unit =
    println(numSubarrayProductLessThanK(Array(10, 5, 2, 6), 100))

  def numSubarrayProductLessThanK(nums: Array[Int], k: Int): Int =
    if k <= 1 then return 0

    var (l, r)         = (0, 0)
    var count          = 0
    var rollingProduct = 1

    nums.indices.foreach { r =>
      rollingProduct *= nums(r)
      while l < nums.length && rollingProduct >= k do
        rollingProduct /= nums(l)
        l += 1
      count += r - l + 1
    }

    count
