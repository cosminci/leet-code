package io.github.cosminci.leetcode._300

import scala.collection.mutable

object _300_LongestIncreasingSubsequence:

  def main(args: Array[String]): Unit =
    println(lengthOfLISDP(Array(10, 9, 2, 5, 3, 7, 101, 18)))
    println(lengthOfLISBinarySearch(Array(10, 9, 2, 5, 3, 7, 101, 18)))

  def lengthOfLISBinarySearch(nums: Array[Int]): Int =
    nums.foldLeft(Seq.fill(nums.length + 1)(Int.MaxValue)) {
      case (stack, n) =>
        stack.updated(stack.search(n).insertionPoint, n)
    }.indexWhere(_ == Int.MaxValue)

  def lengthOfLISDP(nums: Array[Int]): Int =
    val dp = Array.fill[Int](nums.length)(1)
    (1 until nums.length).foreach { end =>
      (0 until end).foreach { start =>
        if nums(end) > nums(start) then dp(end) = math.max(dp(end), dp(start) + 1)
      }
    }
    dp.max
