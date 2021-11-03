package io.github.cosminci.leetcode._200

object _162_FindPeakElement:
  def main(args: Array[String]): Unit =
    println(findPeakElement(Array(1, 2, 3, 1)))
    println(findPeakElement(Array(1, 2, 1, 3, 5, 4, 3)))
    println(findPeakElement(Array(3, 2, 1, 2, 3)))
    println(findPeakElement(Array(1, 2, 3)))
    println(findPeakElement(Array(3, 2, 1)))

  def findPeakElement(nums: Array[Int]): Int =
    var (l, r) = (0, nums.length - 1)
    while l <= r do
      val mid   = (l + r) / 2
      val beforeMid  = if mid >= 1 then nums(mid - 1) else Int.MinValue
      val afterMid = if mid < nums.length - 1 then nums(mid + 1) else Int.MinValue
      if beforeMid < nums(mid) && afterMid < nums(mid) then return mid
      if beforeMid > nums(mid) then r = mid
      else l = mid + 1
    0
