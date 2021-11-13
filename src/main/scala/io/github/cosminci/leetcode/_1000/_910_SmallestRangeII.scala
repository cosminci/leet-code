package io.github.cosminci.leetcode._1000

object _910_SmallestRangeII {
  def smallestRangeII(nums: Array[Int], k: Int): Int = {
    nums.sortInPlace()
    (0 until nums.length - 1).foldLeft(nums.last - nums.head) { (prevResult, i) =>
      prevResult.min((nums(i) + k).max(nums.last - k) - (nums(i + 1) - k).min(nums.head + k))
    }
  }
}
