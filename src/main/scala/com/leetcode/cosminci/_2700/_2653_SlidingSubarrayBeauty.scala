package com.leetcode.cosminci._2700

object _2653_SlidingSubarrayBeauty:

  def getSubarrayBeauty(nums: Array[Int], k: Int, x: Int): Array[Int] =
    val counter = Array.fill(50)(0)
    val result  = Array.fill(nums.length - k + 1)(0)

    @annotation.tailrec
    def xthSmallest(j: Int, total: Int): Int =
      if j == 50 then 0
      else if total + counter(j) >= x then j - 50
      else xthSmallest(j + 1, total + counter(j))

    nums.indices.foreach { i =>
      if nums(i) < 0 then counter(nums(i) + 50) += 1
      if i - k >= 0 && nums(i - k) < 0 then counter(nums(i - k) + 50) -= 1
      if i - k + 1 >= 0 then result(i - k + 1) = xthSmallest(j = 0, total = 0)
    }

    result
