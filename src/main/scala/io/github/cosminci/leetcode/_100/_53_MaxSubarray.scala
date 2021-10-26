package io.github.cosminci.leetcode._100

object _53_MaxSubarray:
  def maxSubArray(nums: Array[Int]): Int =
    if nums.length == 1 then return nums.head

    var max = nums.head
    var sum = nums.head
    nums.tail.foreach { n =>
      sum = math.max(n, sum + n)
      max = math.max(max, sum)
    }

    max
