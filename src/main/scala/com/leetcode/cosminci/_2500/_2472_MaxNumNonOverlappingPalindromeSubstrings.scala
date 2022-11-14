package com.leetcode.cosminci._2500

object _2472_MaxNumNonOverlappingPalindromeSubstrings:

  def maxPalindromes(s: String, k: Int): Int =
    @annotation.tailrec
    def dfs(intervals: Array[(Int, Int)], left: Int, right: Int): Array[(Int, Int)] =
      if left < 0 || right >= s.length || s(left) != s(right) then intervals
      else if right - left + 1 < k then dfs(intervals, left - 1, right + 1)
      else intervals :+ (left, right + 1)

    (0 until 2 * s.length - 1)
      .foldLeft(Array.empty[(Int, Int)]) { (intervals, center) =>
        dfs(intervals, center / 2, center / 2 + center % 2)
      }
      .foldLeft(0, Int.MinValue) { case ((cnt, last), (x, y)) =>
        if x >= last then (cnt + 1, y)
        else if y < last then (cnt, y)
        else (cnt, last)
      }._1
