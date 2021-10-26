package io.github.cosminci.leetcode._600

object _581_ShortestUnsortedContinousSubarray:
  def main(args: Array[String]): Unit =
    Seq(
      Array(1, 2, 3, 3, 3),
      Array(1, 3, 2, 2, 2),
      Array(2, 6, 4, 8, 10, 9, 15)
    ).foreach { nums =>
      println(findUnsortedSubarraySort(nums))
      println(findUnsortedSubarrayPointers(nums))
    }

  private def findUnsortedSubarraySort(nums: Array[Int]): Int =
    val subarray = nums.zip(nums.sorted).zipWithIndex.collect {
      case ((a, b), i) if a != b => i
    }
    if subarray.isEmpty then 0 else subarray.last - subarray.head + 1

  private def findUnsortedSubarrayPointers(nums: Array[Int]): Int =
    if nums.length < 2 then return 0

    var (l, r) = (0, nums.length - 1)
    while l < nums.length - 1 && nums(l) <= nums(l + 1) do l += 1
    while r > 0 && nums(r) >= nums(r - 1) do r -= 1

    if l > r then return 0

    val subarray = nums.slice(l, r + 1)
    var min      = subarray.min
    var max      = subarray.max

    while l > 0 && min < nums(l - 1) do l -= 1
    while r < nums.length - 1 && max > nums(r + 1) do r += 1

    r - l + 1
