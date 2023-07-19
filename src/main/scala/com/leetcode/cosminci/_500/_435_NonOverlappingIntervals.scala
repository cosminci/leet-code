package com.leetcode.cosminci._500

import scala.util.chaining.*

object _435_NonOverlappingIntervals:

  def eraseOverlapIntervals(intervals: Array[Array[Int]]): Int =
    intervals
      .sortBy(_.last).tail
      .foldLeft(0, intervals.minBy(_.last)) { case ((cnt, prev), curr) =>
        if curr.head >= prev.last then (cnt, curr)
        else (cnt + 1, prev)
      }
      .pipe { case (cnt, _) => cnt }
