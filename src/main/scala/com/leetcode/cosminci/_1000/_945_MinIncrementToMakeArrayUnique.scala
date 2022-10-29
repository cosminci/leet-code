package com.leetcode.cosminci._1000

object _945_MinIncrementToMakeArrayUnique {
  def minIncrementForUnique(nums: Array[Int]): Int =
    nums.sorted.foldLeft(0, 0) {
      case ((result, next), n) =>
        (result + (next - n).max(0), next.max(n) + 1)
    }._1
}
