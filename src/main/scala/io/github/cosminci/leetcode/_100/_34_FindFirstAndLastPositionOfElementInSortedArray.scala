package io.github.cosminci.leetcode._100

object _34_FindFirstAndLastPositionOfElementInSortedArray:
  def main(args: Array[String]): Unit =
    println(searchRange(Array(), 0).toList)
    println(searchRange(Array(7, 7, 7, 8, 8, 10, 10, 10), 8).toList)

  private def searchRange(nums: Array[Int], target: Int): Array[Int] =
    def minIdx =
      var (l, r) = (0, nums.length)
      while l < r do
        val mid = l + (r - l) / 2
        if nums(mid) >= target then r = mid
        else l = mid + 1
      if l == nums.length || nums(l) != target then -1 else l

    def maxIdx =
      var (l, r) = (0, nums.length)
      while l < r do
        val mid = l + (r - l) / 2
        if nums(mid) <= target then l = mid + 1
        else r = mid
      if nums(l - 1) == target then l - 1 else -1

    val min = minIdx
    if min == -1 then Array(-1, -1)
    else Array(min, maxIdx)
