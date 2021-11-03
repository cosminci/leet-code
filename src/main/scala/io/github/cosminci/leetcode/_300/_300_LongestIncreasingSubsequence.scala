package io.github.cosminci.leetcode._300

import scala.collection.mutable

object _300_LongestIncreasingSubsequence:

  def main(args: Array[String]): Unit =
    println(lengthOfLISDP(Array(10, 9, 2, 5, 3, 7, 101, 18)))
    println(lengthOfLISBinarySearch(Array(10, 9, 2, 5, 3, 7, 101, 18)))

  def lengthOfLISBinarySearch(nums: Array[Int]): Int =
    val stacks    = Array.ofDim[Int](nums.length)
    var maxLength = 0
    nums.foreach { n =>
      var (l, r) = (0, maxLength)
      while l < r do
        val mid = l + (r - l) / 2
        if stacks(mid) >= n then r = mid
        else l = mid + 1
      stacks(l) = n
      if l == maxLength then maxLength += 1
    }
    maxLength

  def lengthOfLISDP(nums: Array[Int]): Int =
    val dp = Array.fill[Int](nums.length)(1)
    (1 until nums.length).foreach { end =>
      (0 until end).foreach { start =>
        if nums(end) > nums(start) then dp(end) = math.max(dp(end), dp(start) + 1)
      }
    }
    dp.max
