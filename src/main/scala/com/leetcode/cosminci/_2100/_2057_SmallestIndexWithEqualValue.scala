package com.leetcode.cosminci._2100

object _2057_SmallestIndexWithEqualValue {
  def smallestEqual(nums: Array[Int]): Int =
    nums.indices.find(i => i % 10 == nums(i)).getOrElse(-1)
}
