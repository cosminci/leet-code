package com.leetcode.cosminci._300

object _228_SummaryRanges:
  def summaryRanges(nums: Array[Int]): List[String] =
    nums
      .foldLeft(List.empty[(Int, Int)]) { (ranges, n) =>
        ranges match
          case (l, r) :: tail if r + 1 == n => (l, n) :: tail
          case _                            => (n, n) :: ranges
      }
      .reverse
      .map { case (l, r) => if l == r then s"$l" else s"$l->$r" }
