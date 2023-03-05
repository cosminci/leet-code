package com.leetcode.cosminci._2600

object _2580_CountWaysToGroupOverlappingRanges:

  def countWays(ranges: Array[Array[Int]]): Int =
    ranges.sortBy(_.head)
      .foldLeft(1, -1) { case ((res, last), Array(l, r)) =>
        if last >= l then (res, last.max(r))
        else (res * 2 % 1_000_000_007, last.max(r))
      }._1
