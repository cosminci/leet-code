package io.github.cosminci.leetcode._1500

object _1465_MaxAreaOfCakeAfterCuts:

  def maxArea(h: Int, w: Int, horizontalCuts: Array[Int], verticalCuts: Array[Int]): Int =
    def maxCut(cuts: Array[Int], bound: Int): Long =
      (0 +: cuts :+ bound).sorted
        .sliding(2)
        .map { case Array(l, h) => h - l }
        .max

    ((maxCut(horizontalCuts, h) * maxCut(verticalCuts, w)) % 1_000_000_007).toInt
