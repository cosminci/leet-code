package io.github.cosminci.leetcode._1600

object _1509_MinimumDifferenceBetweenLargestAndSmallestValueInThreeMoves:
  def main(args: Array[String]): Unit =
    println(minDifference(Array(9, 48, 92, 48, 81, 31)))
    println(minDifference(Array(82, 81, 95, 75, 20)))

  def minDifference(input: Array[Int]): Int =
    if input.length <= 4 then return 0
    val nums = input.sorted
    (0 to 3).foldLeft(Int.MaxValue) { case (minDiff, toTake) =>
      val diff = nums(nums.length - toTake - 1) - nums(3 - toTake)
      if diff < minDiff then diff else minDiff
    }
