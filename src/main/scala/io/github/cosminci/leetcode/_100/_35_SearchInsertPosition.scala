package io.github.cosminci.leetcode._100

object _35_SearchInsertPosition:
  def main(args: Array[String]): Unit =
    println(searchInsert(Array(1, 3, 5, 6), 5))

  private def searchInsert(nums: Array[Int], target: Int): Int =
    var (l, r) = (0, nums.length)
    while l < r do
      val mid = l + (r - l) / 2
      if nums(mid) >= target then r = mid
      if nums(mid) < target then l = mid + 1
    l
