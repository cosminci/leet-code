package com.leetcode.cosminci._100

object _62_UniquePaths:
  def uniquePaths(m: Int, n: Int): Int =
    (0 until n - 1).foldLeft(Seq.fill(m)(1)) {
      (acc, _) => acc.scanLeft(0)(_ + _)
    }.last
