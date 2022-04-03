package io.github.cosminci.leetcode._2300

object _2222_NumberOfWaysToSelectBuildings:
  def numberOfWays(s: String): Long =
    s.foldLeft(0L, 0L, 0L, s.count(_ == '1'), s.count(_ == '0')) {
      case ((ways, onesBefore, zeroesBefore, onesAfter, zeroesAfter), n) =>
        if n == '1' then (ways + zeroesBefore * zeroesAfter, onesBefore + 1, zeroesBefore, onesAfter - 1, zeroesAfter)
        else (ways + onesBefore * onesAfter, onesBefore, zeroesBefore + 1, onesAfter, zeroesAfter - 1)
    }._1
