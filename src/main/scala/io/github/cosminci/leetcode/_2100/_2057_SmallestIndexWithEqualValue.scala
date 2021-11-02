package io.github.cosminci.leetcode._2100

object _2057_SmallestIndexWithEqualValue {
  private def smallestEqual(nums: Array[Int]): Int =
    nums.indices.find(i => i % 10 == nums(i)).getOrElse(-1)
}
