package io.github.cosminci.leetcode._500

object _485_MaxConsectiveOnes {
  private def findMaxConsecutiveOnesFold(nums: Array[Int]): Int =
    nums.foldLeft(0, 0) {
      case ((max, curr), n) =>
        if (n == 1) (math.max(max, curr + 1), curr + 1) else (max, 0)
    }._1

  private def findMaxConsecutiveOnesScan(nums: Array[Int]): Int =
    nums.scanLeft(0) { case (curr, n) => if (n == 1) curr + 1 else 0 }.max
}
