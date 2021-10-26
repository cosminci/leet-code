package io.github.cosminci.leetcode._400

object _376_WiggleSubsequence:
  def main(args: Array[String]): Unit =
    println(wiggleMaxLength(Array(1, 17, 5, 10, 13, 15, 10, 5, 16, 8)))

  private def wiggleMaxLength(nums: Array[Int]): Int =
    (1 until nums.length)
      .foldLeft(Seq(1, 1)) { case (Seq(prevLongestAsc, prevLongestDesc), i) =>
        if nums(i) > nums(i - 1) then Seq(prevLongestDesc + 1, prevLongestDesc)
        else if nums(i) < nums(i - 1) then Seq(prevLongestAsc, prevLongestAsc + 1)
        else Seq(prevLongestAsc, prevLongestDesc)
      }
      .max
