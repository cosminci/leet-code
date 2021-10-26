package io.github.cosminci.leetcode._700

import scala.collection.mutable

object _673_NumberOfLongestIncreasingSubsequence:
  def main(args: Array[String]): Unit =
    println(findNumberOfLIS(Array(1, 3, 5, 4, 7)))
    println(findNumberOfLIS(Array(2, 2, 2, 2, 2)))

  case class LengthCount(length: Int, count: Int)
  def findNumberOfLIS(nums: Array[Int]): Int =
    val lengths = Array.fill(nums.length)(1)
    val counts  = Array.fill(nums.length)(1)

    (1 until nums.length).foreach { end =>
      (0 until end).foreach { start =>
        if nums(end) > nums(start) then
          if lengths(start) + 1 > lengths(end) then
            lengths(end) = lengths(start) + 1
            counts(end) = counts(start)
          else if lengths(start) + 1 == lengths(end) then counts(end) += counts(start)
      }
    }

    lengths.indices.filter(i => lengths(i) == lengths.max).map(counts).sum
