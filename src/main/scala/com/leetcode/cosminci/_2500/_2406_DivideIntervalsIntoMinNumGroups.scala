package com.leetcode.cosminci._2500

object _2406_DivideIntervalsIntoMinNumGroups:

  def minGroups(intervals: Array[Array[Int]]): Int =
    intervals
      .flatMap { case Array(start, end) => Array((start, 1), (end + 1, -1)) }
      .sorted
      .foldLeft(0, 0) { case ((prevOverlap, maxOverlap), (_, balance)) =>
        (prevOverlap + balance, maxOverlap.max(prevOverlap + balance))
      }._2
