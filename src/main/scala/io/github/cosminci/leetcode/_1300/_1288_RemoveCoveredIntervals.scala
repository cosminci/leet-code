package io.github.cosminci.leetcode._1300

object _1288_RemoveCoveredIntervals:

  def removeCoveredIntervals(intervals: Array[Array[Int]]): Int =
    intervals.length - intervals
      .sortBy(i => (i.head, -i.last))
      .foldLeft(-1, 0) { case ((maxEnd, removed), Array(start, end)) =>
        if end <= maxEnd then (maxEnd, removed + 1)
        else (end, removed)
      }._2
