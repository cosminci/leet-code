package com.leetcode.cosminci._600

object _539_MinTimeDiff {
  def findMinDifference(timePoints: List[String]): Int = {
    if (timePoints.distinct.length != timePoints.length) return 0
    val sorted = timePoints.map { s =>
      val Array(hour, minute) = s.split(':').map(_.toInt)
      hour * 60 + minute
    }.sorted

    val pastMidnightMin = (0 until timePoints.length - 1).map(i => sorted(i + 1) - sorted(i)).min
    val crossMidnightMid = sorted.head + (24 * 60 - sorted.last)

    math.min(crossMidnightMid, pastMidnightMin)
  }
}
