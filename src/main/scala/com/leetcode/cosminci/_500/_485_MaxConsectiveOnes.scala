package com.leetcode.cosminci._500

object _485_MaxConsectiveOnes {
  def findMaxConsecutiveOnesFold(nums: Array[Int]): Int =
    nums.foldLeft(0, 0) {
      case ((maxCount, currCount), n) =>
        if (n == 1) (maxCount.max(currCount + 1), currCount + 1) else (maxCount, 0)
    }._1

  def findMaxConsecutiveOnesScan(nums: Array[Int]): Int =
    nums.scanLeft(0)((currCount, n) => if (n == 1) currCount + 1 else 0).max
}
