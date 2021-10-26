package io.github.cosminci.leetcode._2100

import scala.collection.Searching.{Found, InsertionPoint}

object _2035_PartitionArrayIntoTwoArraysToMinimizeSumDiff:
  def main(args: Array[String]): Unit =
    println(minimumDifference(Array(3, 9, 7, 3)))
    println(minimumDifference(Array(-36, 36)))
    println(minimumDifference(Array(2, -1, 0, 4, -2, -9)))

  def minimumDifference(nums: Array[Int]): Int =
    val n             = nums.length / 2
    val (left, right) = nums.splitAt(n)
    val (lSum, rSum)  = (left.sum, right.sum)
    var minDiff       = Int.MaxValue

    (0 to n / 2).foreach { i =>
      val leftDiffs = left.combinations(i).map(_.sum * 2 - lSum).toArray.sorted

      right.combinations(n - i).foreach { rightCombo =>
        val targetDiff  = 2 * rightCombo.sum - rSum
        val leftDiffIdx = leftDiffs.search(-targetDiff).insertionPoint

        if leftDiffIdx > 0 then
          minDiff = math.min(minDiff, math.abs(leftDiffs(leftDiffIdx - 1) + targetDiff))
        if leftDiffIdx < leftDiffs.length then
          minDiff = math.min(minDiff, math.abs(leftDiffs(leftDiffIdx) + targetDiff))
      }
    }
    minDiff
