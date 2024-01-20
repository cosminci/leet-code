package com.leetcode.cosminci._3000

object _2975_MaxSquareAreaByRemovingFencesFromField:

  def maximizeSquareArea(m: Int, n: Int, hFences: Array[Int], vFences: Array[Int]): Int =
    val hLengths = (Array(1, m) ++ hFences).combinations(2).map(pair => (pair(1) - pair(0)).abs).toSet
    val vLengths = (Array(1, n) ++ vFences).combinations(2).map(pair => (pair(1) - pair(0)).abs).toSet
    hLengths
      .intersect(vLengths)
      .maxOption
      .map(len => ((len.toLong * len) % 1_000_000_007).toInt)
      .getOrElse(-1)
